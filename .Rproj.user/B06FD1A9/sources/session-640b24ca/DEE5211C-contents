# ==========================================
# PROYECTO BI: SUPERSTORE DASHBOARD
# Archivo unificado: app.R
# ==========================================

# 1. CARGA DE LIBRERÍAS
# ------------------------------------------
library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(plotly)
library(scales)
library(caret)
library(lubridate)
library(janitor)

# Aumentar el tamaño máximo de subida (por si acaso)
options(shiny.maxRequestSize = 30*1024^2)

# ==========================================
# 2. ETL Y MODELADO (GLOBAL)
# ==========================================

# --- 2.1 EXTRACT (Carga) ---
# Verifica la ruta del archivo
file_path <- "data/train.csv"
if (!file.exists(file_path)) {
  stop("ERROR CRÍTICO: No se encuentra 'data/train.csv'. Por favor verifica la ruta.")
}

raw_data <- read_csv(file_path, show_col_types = FALSE)

# --- 2.2 TRANSFORM (Staging) ---
df_staging <- raw_data %>%
  clean_names() %>%
  mutate(
    # Surrogate Key
    transaccion_key = row_number(),
    # Formato Fechas (Manejo robusto de formatos)
    order_date = dmy(order_date),
    ship_date = dmy(ship_date),
    # Asegurar numéricos
    sales = as.numeric(sales),
    postal_code = as.character(postal_code)
  ) %>%
  filter(!is.na(sales), !is.na(order_date)) # Limpieza de nulos críticos

# --- 2.3 LOAD (Data Warehouse - Esquema Estrella Lógico) ---

# Creación de dimensiones (Simuladas para Shiny)
dim_cliente <- df_staging %>% distinct(customer_id, customer_name, segment)
dim_ubicacion <- df_staging %>% distinct(country, city, state, region)
dim_producto <- df_staging %>% distinct(product_id, category, sub_category, product_name)

# Data Mart (Tabla desnormalizada para alto rendimiento en Dashboard)
df_mart <- df_staging %>%
  mutate(
    anio = year(order_date),
    mes = month(order_date, label = TRUE, abbr = TRUE),
    dia_semana = wday(order_date, label = TRUE)
  )

# --- 2.4 INFERENCIA ESTADÍSTICA (Modelo) ---
# Modelo de Regresión Lineal Múltiple
set.seed(123)
modelo_lm <- lm(sales ~ category + region + segment, data = df_mart)

# ANOVA (Para cumplir con la rúbrica de inferencia)
anova_region <- aov(sales ~ region, data = df_mart)

# Funciones de Predicción
predecir_ventas <- function(cat, reg, seg) {
  nd <- data.frame(category = cat, region = reg, segment = seg)
  # Usar tryCatch para evitar caídas si hay niveles nuevos
  tryCatch({
    pred <- predict(modelo_lm, newdata = nd)
    return(max(0, pred))
  }, error = function(e) return(0))
}

# Pre-calcular listas para la UI (Evita el error de inputs vacíos)
lista_regiones <- sort(unique(df_mart$region))
lista_categorias <- sort(unique(df_mart$category))
lista_segmentos <- sort(unique(df_mart$segment))


# ==========================================
# 3. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- page_navbar(
  title = "📊 Superstore Analytics Suite",
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  
  # --- PESTAÑA 1: GESTIÓN Y KPIs ---
  nav_panel(
    "1. Tablero de Mando",
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Ventas Totales",
        value = textOutput("kpi_total"),
        showcase = bsicons::bs_icon("cash-coin"),
        theme = "primary"
      ),
      value_box(
        title = "Ticket Promedio",
        value = textOutput("kpi_promedio"),
        showcase = bsicons::bs_icon("receipt"),
        theme = "info"
      ),
      value_box(
        title = "Meta Anual (500k)",
        value = textOutput("kpi_meta"),
        showcase = bsicons::bs_icon("bullseye"),
        theme = "success"
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Tendencia Histórica de Ventas"),
      plotlyOutput("plot_tendencia", height = "400px")
    )
  ),
  
  # --- PESTAÑA 2: ANALÍTICA (DW) ---
  nav_panel(
    "2. Data Mart & Insights",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros Dimensión",
        # Usamos las listas pre-calculadas aquí directamente
        selectInput("filtro_region", "Región:", choices = c("Todas", lista_regiones)),
        selectInput("filtro_categoria", "Categoría:", choices = c("Todas", lista_categorias)),
        hr(),
        helpText("Análisis multidimensional sobre el Data Warehouse.")
      ),
      layout_columns(
        card(card_header("Top Sub-Categorías (Pareto)"), plotlyOutput("plot_barras")),
        card(card_header("Distribución por Segmento"), plotlyOutput("plot_torta"))
      )
    )
  ),
  
  # --- PESTAÑA 3: CIENCIA DE DATOS (ML + Inferencia) ---
  nav_panel(
    "3. Laboratorio de Modelos",
    layout_sidebar(
      sidebar = sidebar(
        title = "Simulador Predictivo",
        selectInput("in_cat", "Categoría:", choices = lista_categorias),
        selectInput("in_reg", "Región:", choices = lista_regiones),
        selectInput("in_seg", "Segmento:", choices = lista_segmentos),
        actionButton("btn_calc", "Predecir Venta", class = "btn-lg btn-primary w-100")
      ),
      # Contenido Principal
      navset_card_underline(
        nav_panel("Predicción", 
                  div(style = "text-align: center; padding: 40px;",
                      h4("Venta Esperada (Regresión)"),
                      h1(textOutput("res_prediccion"), style = "color: #27ae60; font-weight: bold; font-size: 3.5rem;"),
                      p("Basado en modelo lineal histórico.")
                  )
        ),
        nav_panel("Inferencia Estadística (Rúbrica)", 
                  h5("Análisis de Varianza (ANOVA)"),
                  p("Hipótesis: ¿Existe diferencia significativa en las ventas medias entre regiones?"),
                  verbatimTextOutput("stats_anova"),
                  h5("Resumen del Modelo de Regresión"),
                  verbatimTextOutput("stats_model")
        )
      )
    )
  )
)

# ==========================================
# 4. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # --- 4.1 REACTIVIDAD (Filtros) ---
  datos_filtrados <- reactive({
    req(df_mart) # Asegura que los datos existan
    d <- df_mart
    if (input$filtro_region != "Todas") {
      d <- d %>% filter(region == input$filtro_region)
    }
    if (input$filtro_categoria != "Todas") {
      d <- d %>% filter(category == input$filtro_categoria)
    }
    return(d)
  })
  
  # --- 4.2 KPIs ---
  output$kpi_total <- renderText({ dollar(sum(df_mart$sales)) })
  output$kpi_promedio <- renderText({ dollar(mean(df_mart$sales)) })
  output$kpi_meta <- renderText({
    ventas_totales <- sum(df_mart$sales)
    # Supongamos que son datos de 4 años, meta anual promedio 500k -> total 2M
    meta_total <- 2000000 
    pct <- (ventas_totales / meta_total) * 100
    paste0(round(pct, 1), "% (Global)")
  })
  
  # --- 4.3 GRÁFICOS ---
  output$plot_tendencia <- renderPlotly({
    # Agrupamos por mes para suavizar y evitar errores de muchos puntos
    g <- df_mart %>%
      group_by(mes_anio = floor_date(order_date, "month")) %>%
      summarise(ventas = sum(sales)) %>%
      ggplot(aes(x = mes_anio, y = ventas)) +
      geom_line(color = "#007bc2") +
      geom_area(fill = "#007bc2", alpha = 0.1) +
      geom_smooth(method = "loess", color = "darkblue", se = FALSE, span = 0.3) +
      scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
      theme_minimal() +
      labs(x = "Fecha", y = "Ventas Mensuales")
    
    ggplotly(g)
  })
  
  output$plot_barras <- renderPlotly({
    d <- datos_filtrados()
    # Validación: Si no hay datos, mostrar mensaje vacío
    validate(need(nrow(d) > 0, "No hay datos para estos filtros"))
    
    g <- d %>%
      group_by(sub_category) %>%
      summarise(total = sum(sales)) %>%
      arrange(desc(total)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(sub_category, total), y = total, fill = sub_category)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      theme_light() +
      theme(legend.position = "none") +
      labs(x = "", y = "Ventas")
    
    ggplotly(g)
  })
  
  output$plot_torta <- renderPlotly({
    d <- datos_filtrados()
    validate(need(nrow(d) > 0, "No hay datos"))
    
    resumen <- d %>%
      group_by(segment) %>%
      summarise(total = sum(sales))
    
    plot_ly(resumen, labels = ~segment, values = ~total, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c("#3498db", "#e74c3c", "#f1c40f"))) %>%
      layout(showlegend = FALSE)
  })
  
  # --- 4.4 ML & ESTADÍSTICA ---
  
  # Predicción
  prediccion <- eventReactive(input$btn_calc, {
    predecir_ventas(input$in_cat, input$in_reg, input$in_seg)
  })
  
  output$res_prediccion <- renderText({
    req(prediccion())
    dollar(prediccion())
  })
  
  # Reportes Estadísticos (Para Rubrica Punto 2)
  output$stats_anova <- renderPrint({
    print(summary(anova_region))
    cat("\nINTERPRETACIÓN:\nSi Pr(>F) es < 0.05, hay diferencias significativas de ventas entre regiones.")
  })
  
  output$stats_model <- renderPrint({
    summary(modelo_lm)
  })
}

# ==========================================
# 5. EJECUCIÓN
# ==========================================
shinyApp(ui = ui, server = server)