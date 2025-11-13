# server.R (Código FINAL y ESTABILIZADO)

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2) 
library(shinyjs) 
library(readr) # Necesario para leer archivos CSV
library(readxl) # Para leer el archivo Excel
library(tibble) # Necesario para crear tibbles de emergencia

# --- 1. LECTURA DE DATOS DESDE ARCHIVOS CSV Y EXCEL ---
tryCatch({
  datos_banano_raw <- read_excel("Racimos Cosechados Semana 45-1 - 2025 - Total.xlsx") 
  user_base <- read.csv("usuarios.csv", sep = ";", stringsAsFactors = FALSE)
  
}, error = function(e) {
  warning(paste("Error al cargar o procesar archivos:", e$message))
  
  # Carga datos vacíos para que la app no colapse
  datos_banano_raw <<- data.frame(EMPRESA=character(), Lote=character(), PESO=numeric())
  user_base <<- tibble::tibble(user = "error@db.com", permissions = "SUPER_ADMIN", empresa_id = "ALL", name = "Error DB")
})


server <- function(input, output, session) {
  
  user_email_js <- reactiveVal(NULL)
  
  # --- A. INICIALIZACIÓN DE FIREBASE Y COMPONENTES JS ---
  observe({
    api_key <- "AIzaSyC20-K42ErsY-bKKeHKBxIecJ6FaXbadXw" 
    auth_domain <- "bdspb-f17f3.firebaseapp.com" 
    
    js_init <- sprintf("
      var firebaseConfig = { 
        apiKey: '%s', 
        authDomain: '%s' 
      }; 
      var app = firebase.initializeApp(firebaseConfig);
      var auth = firebase.auth();
      
      auth.setPersistence(firebase.auth.Auth.Persistence.SESSION);

      auth.onAuthStateChanged(function(user) {
        if (user) {
          Shiny.setInputValue('firebase_user_email', user.email);
          document.getElementById('login_panel').style.display = 'none';
        } else {
          Shiny.setInputValue('firebase_user_email', null);
          document.getElementById('login_panel').style.display = 'block';
        }
      });

      $('#login_submit').on('click', function() {
        var email = $('#login_email').val();
        var password = $('#login_password').val();
        
        auth.signInWithEmailAndPassword(email, password)
          .catch(function(error) {
            if (error.code === 'auth/user-not-found') {
                auth.createUserWithEmailAndPassword(email, password)
                    .then(function() {
                        Shiny.setInputValue('login_status', 'Registro exitoso.', {priority: 'event'});
                    })
                    .catch(function(error) {
                        Shiny.setInputValue('login_status', error.message, {priority: 'event'});
                    });
            } else {
                Shiny.setInputValue('login_status', error.message, {priority: 'event'});
            }
          });
      });
      
      Shiny.addCustomMessageHandler('sign_out', function(message) {
          auth.signOut();
      });
    ", api_key, auth_domain)
    
    shinyjs::runjs(js_init)
  })
  
  # --- B. LÓGICA DE LOGIN Y LOGOUT EN R ---
  
  observeEvent(input$firebase_user_email, {
    user_email_js(input$firebase_user_email)
  })
  
  observeEvent(input$login_status, {
    showNotification(input$login_status, type = "error", duration = 5)
  })
  
  observeEvent(input$logout_btn, {
    session$sendCustomMessage(type = 'sign_out', message = list())
  })
  
  # --- 4. OBTENER INFORMACIÓN DEL USUARIO Y MAPEO DE ROLES ---
  user_info <- reactive({
    current_user_email <- user_email_js() 
    req(current_user_email) 
    
    user_data <- user_base %>%
      filter(user == current_user_email) %>%
      {
        if (nrow(.) == 0) {
          tibble(
            user = current_user_email,
            permissions = "OPERADOR_CADENA", 
            empresa_id = "EMP_DEFAULT",
            name = sub("@.*", "", current_user_email)
          )
        } else {
          .
        }
      }
    
    list(
      logged_in = TRUE,
      user = user_data$user[1],
      role = user_data$permissions[1],
      empresa_id = user_data$empresa_id[1],
      name = user_data$name[1]
    )
  })
  
  # --- 5. FILTRADO DE DATOS CRUCOS POR EMPRESA (Reactivo) ---
  
  datos_filtrados_crudos <- reactive({
    user <- user_info()
    req(user)
    
    req(nrow(datos_banano_raw) > 0)
    
    if (user$role == "SUPER_ADMIN") {
      return(datos_banano_raw)
    } else {
      # Filtra datos solo para la empresa del usuario
      return(datos_banano_raw %>% filter(EMPRESA == user$empresa_id))
    }
  })
  
  # --- 5B. Lógica de Mapeo y Preparación de Columnas (Estabilización) ---
  datos_dashboard <- reactive({
    data <- datos_filtrados_crudos()
    req(nrow(data) > 0)
    
    
    
    
    # *** CORRECCIÓN DE COLUMNAS Y PREPARACIÓN MÍNIMA ***
    data %>%
      # 1. Renombramos las columnas con nombres problemáticos a nombres internos sin espacios
      rename(
        LOTE_ID = Lote, 
        PESO_BRUTO = `Peso bruto`, # ¡CORREGIDO!
        CALIBRACION_SUP = `Calibracion superior`, # ¡CORREGIDO!
        SEMANA_COSECHA = `Semana de cosecha`, # ¡CORREGIDO!
        EMPRESA_ID_FILTRO = EMPRESA
        # Asumiendo que las columnas RECUSADOS y TASA_RECHAZO existen o se definen en el Excel/raw
        # Si no existen, los KPIs de Rechazo seguirán fallando.
      ) %>%
      mutate(
        # 2. Aseguramos tipos de datos
        Ano = as.numeric(Ano),
        LOTE_ID = as.character(LOTE_ID),
        # AHORA CONVERTIMOS SEMANA_COSECHA A CHARACTER/FACTOR para el filtro
        SEMANA_COSECHA = as.character(SEMANA_COSECHA)
      ) %>%
        
      
      # 3. Seleccionamos solo las columnas críticas para la agrupación y cálculo
      select(
        EMPRESA_ID_FILTRO, HACIENDA, Ano, LOTE_ID, SEMANA_COSECHA, Edad, Cinta, 
        PESO_BRUTO, CALIBRACION_SUP, # Columnas estandarizadas
        # Mantener otras columnas necesarias para los KPIs/Tablas existentes
        `Peso raquis`, Rechazado, Recuperado, `Numero de manos`, palanca, Defecto, `Generador de merma`, EdDi, `Tipo de plantacion`, TPId, MC
        # Mantener TASA_RECHAZO y RECUSADOS (si existen en el data.frame)
      )
  })
  
  # --- 8. LÓGICA DEL REPORTE ADMINISTRATIVO (Agrupación por Lote y Promedios Múltiples) ---
  reporte_promedios <- reactive({
    data <- datos_dashboard() 
    req(nrow(data) > 0)
    
    
    # *** 1. APLICAR FILTRO POR SEMANA ***
    if (!is.null(input$filtro_semana) && input$filtro_semana != "Todos") {
      data <- data %>%
        filter(SEMANA_COSECHA == input$filtro_semana)
    }
    
    req(nrow(data) > 0) # Si el filtro no devuelve nada, salimos
  
    
    
    
      # AGRUPACIÓN: EMPRESA, HACIENDA y LOTE
    data %>%
      group_by(
        EMPRESA_ID_FILTRO, # EMPRESA
        HACIENDA, 
        LOTE_ID # LOTE
      ) %>%
      summarise(
        # 1. Promedio del Peso Bruto
        Peso_Bruto_Promedio = mean(PESO_BRUTO, na.rm = TRUE), 
        
        # 2. Promedio de Calibración Superior (Usando el nombre mapeado)
        Calibracion_Promedio = mean(CALIBRACION_SUP, na.rm = TRUE),
        
        # 3. Promedio de Número de Manos (Usando el nombre original de la columna con espacio, ya que no fue mapeada)
        Num_Manos_Promedio = mean(`Numero de manos`, na.rm = TRUE),
        
        # Conteo de Racimos
        Total_Racimos = n(),
        .groups = 'drop'
      ) %>%
      # Ordenar por Hacienda y Lote
      arrange(HACIENDA, LOTE_ID)
  })
  
  
  
  
  # --- 6. RENDERIZACIÓN DEL DASHBOARD Y MENÚ CONDICIONAL ---
  output$sidebar_menu <- renderUI({
    user <- user_info()
    
    logout_button <- actionButton("logout_btn", "Salir", icon = icon("sign-out-alt"), 
                                  style = "color: white; background-color: #d9534f; border-color: #d9534f;")
    
    dashboardPage(
      skin = "green", 
      dashboardHeader(
        title = HTML("🍌 **SISBANLAM** | BI Bananero"),
        titleWidth = 300,
        tags$li(class = "dropdown", logout_button) 
      ),
      dashboardSidebar(
        width = 300, 
        sidebarMenu(
          id = "tabs", 
          # *** PESTAÑA 1 RENOMBRADA Y TABNAME CORREGIDO ***
          menuItem("📊 Reporte Administrativo", tabName = "tab_reporte_admin", icon = icon("chart-bar")),
          
          menuItem("❌ Tasa de Rechazo", tabName = "tab_rechazo", icon = icon("times")),
          menuItem("🔬 Optimización por Edad", tabName = "tab_edad", icon = icon("leaf")),
          
          if (user$role %in% c("SUPER_ADMIN", "ADMIN_EMPRESA")) {
            menuItem("⚙️ Gestión Multi-Empresa", tabName = "tab_admin", icon = icon("user-shield"))
          },
          menuItem("💡 Acerca del Sistema", tabName = "tab_info", icon = icon("info-circle"))
        )
      ),
      dashboardBody(
        h3(paste("Bienvenido,", user$name, " (Rol:", user$role, ")"), icon("hand-peace")),
        
        tabItems(
          # 1. Pestaña de Reporte Administrativo (Antes tab_rendimiento)
          tabItem(tabName = "tab_reporte_admin",
                  h2("Reporte Administrativo: Parámetros de Producción"),
                  
                  fluidRow(
                    valueBoxOutput("kpi_peso_promedio"),
                    valueBoxOutput("kpi_calib_promedio"),
                    valueBoxOutput("kpi_tasa_rechazo"),
                    
                    box(title = "Filtro de Datos", status = "warning", width = 3,
                        uiOutput("ui_filtro_semana") # ESTO HACE LLAMADO A LA LÓGICA REACTIVA ARRIBA
                    )
                    
                  ),
                  # Contenedor para el reporte tabular
                  fluidRow(
                    box(title = "Promedios de Producción por Parámetro", status = "primary", solidHeader = TRUE, width = 12,
                        DT::dataTableOutput("table_promedios"))
                  )
          ),
          
          # 2. Pestaña de Tasa de Rechazo (El tabItem original)
          tabItem(tabName = "tab_rechazo",
                  h2("Análisis de Pérdidas y Recusados"),
                  fluidRow(
                    box(title = "Tasa de Rechazo por Lote (%)", status = "danger", solidHeader = TRUE, width = 12,
                        plotOutput("plot_tasa_rechazo")),
                    box(title = "Detalle de Recusados", status = "warning", width = 12,
                        DT::dataTableOutput("table_rechazo"))
                  )
          ),
          
          # 3. Pestaña de Optimización por Edad 
          tabItem(tabName = "tab_edad",
                  h2("Optimización de Cosecha (Innovación)"),
                  h4("Aquí se implementará el análisis de dispersión para encontrar la edad de corte ideal.")
          ),
          
          # 4. Pestaña de Gestión Multi-Empresa (Solo visible según el rol)
          tabItem(tabName = "tab_admin",
                  h2("⚙️ Gestión de Usuarios y Empresas"),
                  p("Esta sección es visible solo para Administradores de Empresa y Super Administradores."),
                  tags$ul(
                    tags$li("Permitirá la creación y asignación de Subusuarios."),
                    tags$li("Actualmente, los roles se leen desde el archivo **usuarios.csv**.")
                  )
          ),
          
          # 5. Acerca del Sistema
          tabItem(tabName = "tab_info",
                  h2("Acerca de SISBANLAM"),
                  p("Sistema de Inteligencia de Negocios para la Producción Bananera.")
          )
        )
      )
    )
  })
  
  # --- 9. RENDERIZACIÓN DE LA TABLA DEL REPORTE ADMINISTRATIVO ---
  output$table_promedios <- DT::renderDataTable({
    data <- reporte_promedios()
    req(nrow(data) > 0)
    
    DT::datatable(
      data, 
      options = list(pageLength = 10, scrollX = TRUE), 
      rownames = FALSE
    ) %>%
      formatRound(columns = 'Peso_Bruto_Promedio', digits = 2) %>% # 2 decimales para Peso
      formatRound(columns = 'Calibracion_Promedio', digits = 1) %>% # 1 decimal para Calibración (mm)
      formatRound(columns = 'Num_Manos_Promedio', digits = 1)       # 1 decimal para Manos
  })
  
  
  
  
  # --- LÓGICA PARA GENERAR EL FILTRO DE SEMANAS (Añadir a Server.R) ---
  output$ui_filtro_semana <- renderUI({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    
    # Obtener todas las semanas únicas y ordenarlas
    semanas <- unique(data$SEMANA_COSECHA)
    semanas <- sort(semanas)
    
    # Opciones, incluyendo 'Todos'
    opciones <- c("Todos", semanas)
    
    # Generar el control de selección
    selectInput(
      "filtro_semana", 
      "Filtrar por Semana:", 
      choices = opciones,
      selected = "Todos"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  # --- 7. LÓGICA DE KPIS Y GRÁFICOS (CORREGIDA CON NUEVOS NOMBRES) ---
  
  # KPI 1: Peso Promedio
  output$kpi_peso_promedio <- renderValueBox({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando PESO_BRUTO
    promedio <- mean(data$PESO_BRUTO, na.rm = TRUE) 
    valueBox(value = paste(round(promedio, 1), "Kg"), subtitle = "Peso Promedio Global", icon = icon("balance-scale"), color = "navy") 
  })
  
  # KPI 2: Calibración Promedio
  output$kpi_calib_promedio <- renderValueBox({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando CALIBRACION_SUP
    promedio <- mean(data$CALIBRACION_SUP, na.rm = TRUE) 
    valueBox(value = paste(round(promedio, 1), "mm"), subtitle = "Calibracion Promedio", icon = icon("expand"), color = "green")
  })
  
  # KPI 3: Tasa de Rechazo (ASUMIENDO QUE TASA_RECHAZO EXISTE O ESTÁ CALCULADA EN OTRO LUGAR)
  # output$kpi_tasa_rechazo <- renderValueBox({
  # data <- datos_dashboard()
  # req(nrow(data) > 0)
  #  tasa <- mean(data$TASA_RECHAZO, na.rm = TRUE)
  #  valueBox(value = paste(round(tasa, 1), "%"), subtitle = "Tasa de Rechazo Promedio", icon = icon("fire"), color = "red") 
  #})
  
  # Gráfico: Peso Promedio por Lote
  output$plot_peso_lote <- renderPlot({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID y PESO_BRUTO
    data %>% 
      group_by(LOTE_ID) %>% summarise(Peso_Prom = mean(PESO_BRUTO)) %>% 
      ggplot(aes(x = LOTE_ID, y = Peso_Prom)) + 
      geom_bar(stat = "identity", fill = "#2E8B57") + 
      labs(y = "Peso Promedio (Kg)", x = "Lote") + 
      theme_minimal(base_size = 14) 
  })
  
  # Gráfico: Calibración Promedio por Lote
  output$plot_calib_lote <- renderPlot({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID y CALIBRACION_SUP
    data %>% group_by(LOTE_ID) %>% summarise(Calib_Prom = mean(CALIBRACION_SUP)) %>% 
      ggplot(aes(x = LOTE_ID, y = Calib_Prom)) + 
      geom_bar(stat = "identity", fill = "#3c8dbc") + 
      labs(y = "Calibracion Promedio (mm)", x = "Lote") + 
      theme_minimal(base_size = 14)
  })
  
  # Gráfico: Tasa de Rechazo por Lote
 # output$plot_tasa_rechazo <- renderPlot({
 #   data <- datos_dashboard()
 #   req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID
  #  data %>% ggplot(aes(x = LOTE_ID, y = TASA_RECHAZO)) + 
    #  geom_point(size = 3, color = "#d9534f") + 
    #  geom_segment(aes(x = LOTE_ID, xend = LOTE_ID, y = 0, yend = TASA_RECHAZO), color = "#d9534f") + 
    #  labs(y = "Tasa de Rechazo (%)", x = "Lote") + 
    #  theme_minimal(base_size = 14)
  #})
  
  # Tabla: Detalle de Recusados
  output$table_rechazo <- DT::renderDataTable({
    data <- datos_dashboard()
    req(nrow(data) > 0)
    # ¡CORREGIDO! Usando LOTE_ID, PESO_BRUTO, CALIBRACION_SUP
    data %>% select(LOTE_ID, Edad, PESO_BRUTO, CALIBRACION_SUP) %>% 
      DT::datatable(options = list(pageLength = 5), rownames = FALSE)
  })
}