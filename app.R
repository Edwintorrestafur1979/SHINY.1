# =============================================================================
#  Aplicación Shiny: Análisis estadístico descriptivo del dataset "sector"
#  Variables según el diccionario:
#    Nominal  : SECTOR
#    Ordinal  : NIVEL, SATISFACCION, PARTICIPACION
#    Discreta : EXPERIENCIA, HIJOS
#    Continua : SALARIO, LABORAL, EDAD, PROGRAMAS
#
#  Uso: coloque este archivo (app.R) y "sector.xlsx" en la misma carpeta y
#  ejecute  shiny::runApp()  desde esa carpeta (o el botón "Run App" de RStudio).
#  También puede cargar el archivo desde la pestaña "Datos".
# =============================================================================

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)

# -----------------------------------------------------------------------------
# 1. Configuración de variables (según el diccionario)
# -----------------------------------------------------------------------------
niveles_ord <- list(
  NIVEL         = c("Baja", "Media", "Alta"),
  SATISFACCION  = c("Muy Insatisfecho", "Insatisfecho", "Neutral",
                    "Satisfecho", "Muy Satisfecho"),
  PARTICIPACION = c("Nunca", "Rara vez", "A veces", "Frecuentemente", "Siempre")
)

etiquetas <- c(
  SECTOR        = "Sector laboral",
  NIVEL         = "Nivel de ingresos",
  SATISFACCION  = "Satisfacción con el servicio",
  PARTICIPACION = "Frecuencia de participación",
  EXPERIENCIA   = "Años de experiencia",
  HIJOS         = "Número de hijos",
  SALARIO       = "Salario mensual (miles de COP)",
  LABORAL       = "Satisfacción laboral (0.5 a 2.5)",
  EDAD          = "Edad (años)",
  PROGRAMAS     = "Participación en programas (proporción)"
)

tipo_var <- c(
  SECTOR = "Nominal", NIVEL = "Ordinal", SATISFACCION = "Ordinal",
  PARTICIPACION = "Ordinal", EXPERIENCIA = "Discreta", HIJOS = "Discreta",
  SALARIO = "Continua", LABORAL = "Continua", EDAD = "Continua",
  PROGRAMAS = "Continua"
)

# Decimales con que vienen registradas las variables continuas
decimales <- c(SALARIO = 0, LABORAL = 2, EDAD = 0, PROGRAMAS = 3)

var_nom  <- "SECTOR"
var_ord  <- names(niveles_ord)
var_dis  <- c("EXPERIENCIA", "HIJOS")
var_con  <- names(decimales)
var_cual <- c(var_nom, var_ord)

opciones <- function(vars) setNames(vars, paste0(etiquetas[vars], " (", vars, ")"))

# -----------------------------------------------------------------------------
# 2. Funciones auxiliares
# -----------------------------------------------------------------------------
preparar <- function(d) {
  d <- as.data.frame(d)
  d$SECTOR <- factor(d$SECTOR)
  for (v in names(niveles_ord)) d[[v]] <- factor(d[[v]], levels = niveles_ord[[v]])
  d
}

tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

paleta_cual <- function(v) if (v %in% var_ord) "RdYlGn" else "Set2"

# Fila "Total" en blanco salvo las columnas indicadas
agregar_total <- function(df, valores) {
  total <- df[1, , drop = FALSE]
  total[1, ] <- NA
  total[[1]] <- "Total"
  for (n in names(valores)) total[[n]] <- valores[[n]]
  rbind(df, total)
}

# Tabla de frecuencias para variables cualitativas y discretas
frec_categorias <- function(x, nombre = "Categoría", acumuladas = FALSE) {
  t  <- table(x)
  fi <- as.integer(t)
  n  <- sum(fi)
  df <- data.frame(Cat = names(t), fi = fi,
                   hi = round(fi / n, 4),
                   pct = round(100 * fi / n, 2),
                   check.names = FALSE, stringsAsFactors = FALSE)
  if (acumuladas) {
    df$Fi <- cumsum(fi)
    df$Hi <- round(cumsum(fi) / n, 4)
  }
  df <- agregar_total(df, list(fi = n, hi = 1, pct = 100))
  names(df)[1] <- nombre
  names(df)[names(df) == "pct"] <- "pi (%)"
  df
}

# Parámetros de agrupación para variables continuas
param_continua <- function(x, dec, k = NULL) {
  x   <- x[!is.na(x)]
  n   <- length(x)
  f   <- 10^dec
  Min <- min(x)
  Max <- max(x)
  R   <- round(Max - Min, dec)
  if (is.null(k) || is.na(k)) k <- ceiling(1 + 3.322 * log10(n))
  A    <- ceiling(round(R / k * f, 6)) / f
  Rc   <- round(k * A, dec)
  Dif  <- round(Rc - R, dec)
  Xmin <- round(Min - Dif / 2, dec + 1)
  Xmax <- round(Max + Dif / 2, dec + 1)
  list(n = n, Min = Min, Max = Max, R = R, k = k, A = A, Rc = Rc,
       Dif = Dif, Xmin = Xmin, Xmax = Xmax,
       cortes = round(seq(Xmin, by = A, length.out = k + 1), dec + 1))
}

# Tabla de frecuencias por intervalos para variables continuas
frec_continua <- function(x, p, dec, derecha = TRUE) {
  interv <- cut(x, breaks = p$cortes, include.lowest = TRUE,
                right = derecha, dig.lab = 10)
  fi <- as.integer(table(interv))
  n  <- sum(fi)
  li <- head(p$cortes, -1)
  ls <- tail(p$cortes, -1)
  df <- data.frame(Intervalo = levels(interv), Li = li, Ls = ls,
                   mi = round((li + ls) / 2, dec + 2), fi = fi,
                   hi = round(fi / n, 4), Fi = cumsum(fi),
                   Hi = round(cumsum(fi) / n, 4),
                   stringsAsFactors = FALSE)
  agregar_total(df, list(fi = n, hi = 1))
}

# Medidas de resumen para variables cuantitativas
medidas <- function(x, con_moda = TRUE) {
  x <- x[!is.na(x)]
  m <- mean(x); s <- sd(x)
  t <- table(x)
  moda <- if (con_moda) paste(names(t)[t == max(t)], collapse = ", ") else "No aplica (continua)"
  asim <- mean((x - m)^3) / (mean((x - m)^2))^(3 / 2)
  curt <- mean((x - m)^4) / (mean((x - m)^2))^2 - 3
  data.frame(
    Medida = c("n", "Media", "Mediana", "Moda", "Desviación estándar", "Varianza",
               "Coeficiente de variación (%)", "Mínimo", "Q1 (percentil 25)",
               "Q3 (percentil 75)", "Máximo", "Rango intercuartílico",
               "Coeficiente de asimetría", "Curtosis (exceso)"),
    Valor = c(length(x), round(m, 4), median(x), moda, round(s, 4), round(var(x), 4),
              round(100 * s / m, 2), min(x), quantile(x, 0.25), quantile(x, 0.75),
              max(x), IQR(x), round(asim, 4), round(curt, 4)),
    stringsAsFactors = FALSE
  )
}

tabla_dt <- function(df) {
  datatable(df, rownames = FALSE, class = "compact stripe hover",
            options = list(dom = "t", paging = FALSE, ordering = FALSE,
                           columnDefs = list(list(className = "dt-right",
                                                  targets = seq_len(ncol(df) - 1))))) %>%
    formatStyle(names(df)[1], target = "row",
                fontWeight = styleEqual("Total", "bold"),
                backgroundColor = styleEqual("Total", "#dff0d8"))
}

dl_png <- function(grafico, nombre) {
  downloadHandler(
    filename = function() paste0(nombre(), ".png"),
    content  = function(file) ggsave(file, grafico(), width = 10, height = 6, dpi = 150, bg = "white")
  )
}

dl_csv <- function(tabla, nombre) {
  downloadHandler(
    filename = function() paste0(nombre(), ".csv"),
    content  = function(file) write.csv2(tabla(), file, row.names = FALSE, na = "",
                                         fileEncoding = "UTF-8")
  )
}

botones_descarga <- function(id_tabla, id_graf) {
  div(style = "margin-top:10px;",
      downloadButton(id_tabla, "Tabla (CSV)", class = "btn-sm"),
      downloadButton(id_graf, "Gráfico (PNG)", class = "btn-sm"))
}

# -----------------------------------------------------------------------------
# 3. Interfaz de usuario
# -----------------------------------------------------------------------------
ui <- navbarPage(
  title = "Estadística descriptiva | dataset sector",
  header = tags$head(tags$style(HTML("
    .interpretacion {background:#f7f9fc; border-left:4px solid #2c7fb8;
                     padding:10px 14px; margin-top:12px; font-size:14px;}
    h4 {color:#2c3e50; margin-top:18px;}"))),

  # ---- Datos ----------------------------------------------------------------
  tabPanel("Datos",
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("archivo", "Cargar sector.xlsx (opcional)", accept = ".xlsx"),
        helpText("Si no carga un archivo, la aplicación busca 'sector.xlsx'",
                 "en la misma carpeta que app.R."),
        hr(),
        uiOutput("resumen_datos")
      ),
      mainPanel(width = 9,
        h4("Diccionario de variables"),
        tableOutput("diccionario"),
        h4("Vista de los datos"),
        DTOutput("tabla_datos")
      )
    )
  ),

  # ---- Nominal --------------------------------------------------------------
  tabPanel("Nominal",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("v_nom", "Variable nominal", opciones(var_nom)),
        radioButtons("g_nom", "Gráfico",
                     c("Barras verticales" = "barras", "Barras horizontales" = "barrash",
                       "Circular (pastel)" = "pastel")),
        checkboxInput("ordenar_nom", "Ordenar barras por frecuencia", TRUE),
        botones_descarga("dlt_nom", "dlg_nom")
      ),
      mainPanel(width = 9,
        h4("Tabla de frecuencias"), DTOutput("t_nom"),
        h4("Gráfico"), plotOutput("p_nom", height = "430px"),
        uiOutput("i_nom")
      )
    )
  ),

  # ---- Ordinal --------------------------------------------------------------
  tabPanel("Ordinal",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("v_ord", "Variable ordinal", opciones(var_ord)),
        radioButtons("g_ord", "Gráfico",
                     c("Barras" = "barras", "Circular (pastel)" = "pastel",
                       "Ojiva (frecuencia acumulada)" = "ojiva")),
        botones_descarga("dlt_ord", "dlg_ord")
      ),
      mainPanel(width = 9,
        h4("Tabla de frecuencias"), DTOutput("t_ord"),
        h4("Gráfico"), plotOutput("p_ord", height = "430px"),
        uiOutput("i_ord")
      )
    )
  ),

  # ---- Discreta -------------------------------------------------------------
  tabPanel("Discreta",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("v_dis", "Variable discreta", opciones(var_dis)),
        radioButtons("g_dis", "Gráfico",
                     c("Bastones" = "bastones", "Barras" = "barras",
                       "Escalonado (frecuencia acumulada)" = "escalon",
                       "Diagrama de caja" = "caja")),
        botones_descarga("dlt_dis", "dlg_dis")
      ),
      mainPanel(width = 9,
        fluidRow(
          column(8, h4("Tabla de frecuencias"), DTOutput("t_dis")),
          column(4, h4("Medidas de resumen"), DTOutput("m_dis"))
        ),
        h4("Gráfico"), plotOutput("p_dis", height = "430px"),
        uiOutput("i_dis")
      )
    )
  ),

  # ---- Continua -------------------------------------------------------------
  tabPanel("Continua",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("v_con", "Variable continua", opciones(var_con)),
        checkboxInput("sturges", "Número de intervalos por regla de Sturges", TRUE),
        conditionalPanel("!input.sturges",
          numericInput("k_con", "Número de intervalos (k)", 9, min = 3, max = 25)),
        radioButtons("cierre", "Intervalos",
                     c("Cerrados a la derecha (a, b]" = "der",
                       "Cerrados a la izquierda [a, b)" = "izq")),
        radioButtons("g_con", "Gráfico",
                     c("Histograma" = "hist", "Polígono de frecuencias" = "poligono",
                       "Ojiva" = "ojiva", "Densidad" = "densidad",
                       "Diagrama de caja" = "caja")),
        botones_descarga("dlt_con", "dlg_con")
      ),
      mainPanel(width = 9,
        fluidRow(
          column(6, h4("Parámetros de agrupación"), DTOutput("par_con")),
          column(6, h4("Medidas de resumen"), DTOutput("m_con"))
        ),
        h4("Tabla de distribución de frecuencias"), DTOutput("t_con"),
        h4("Gráfico"), plotOutput("p_con", height = "430px"),
        uiOutput("i_con")
      )
    )
  ),

  # ---- Cruce de variables cualitativas ---------------------------------------
  tabPanel("Cruce cualitativas",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("v_fila", "Variable en filas", opciones(var_cual), selected = "SECTOR"),
        selectInput("v_col", "Variable en columnas", opciones(var_cual), selected = "SATISFACCION"),
        radioButtons("tipo_cruce", "Tabla",
                     c("Frecuencias absolutas" = "abs",
                       "Relativas conjuntas (sobre el total)" = "conj",
                       "Relativas por fila" = "fila",
                       "Relativas por columna" = "col")),
        checkboxInput("pct_cruce", "Mostrar relativas en porcentaje", FALSE),
        radioButtons("g_cruce", "Gráfico",
                     c("Barras conjuntas (agrupadas)" = "agrupadas",
                       "Barras apiladas verticales" = "apiladas",
                       "Barras apiladas horizontales" = "apiladash",
                       "Barras apiladas al 100 %" = "cien")),
        botones_descarga("dlt_cruce", "dlg_cruce")
      ),
      mainPanel(width = 9,
        h4("Tabla de contingencia"), DTOutput("t_cruce"),
        h4("Gráfico"), plotOutput("p_cruce", height = "480px"),
        h4("Prueba chi-cuadrado de independencia"),
        uiOutput("chi_cruce")
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 4. Servidor
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Carga de datos -------------------------------------------------------
  datos <- reactive({
    ruta <- if (!is.null(input$archivo)) input$archivo$datapath
            else if (file.exists("sector.xlsx")) "sector.xlsx"
            else NULL
    validate(need(!is.null(ruta),
                  "Cargue el archivo sector.xlsx en la pestaña 'Datos' o colóquelo junto a app.R."))
    d <- read_excel(ruta)
    faltan <- setdiff(names(etiquetas), names(d))
    validate(need(length(faltan) == 0,
                  paste("Faltan columnas en el archivo:", paste(faltan, collapse = ", "))))
    preparar(d)
  })

  output$diccionario <- renderTable({
    data.frame(Variable = names(etiquetas), Descripción = unname(etiquetas),
               Tipo = unname(tipo_var[names(etiquetas)]))
  }, striped = TRUE, bordered = TRUE)

  output$tabla_datos <- renderDT({
    datatable(datos(), rownames = FALSE, class = "compact stripe",
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$resumen_datos <- renderUI({
    d <- datos()
    tagList(p(strong("Observaciones: "), nrow(d)),
            p(strong("Variables: "), ncol(d)),
            p(strong("Datos faltantes: "), sum(is.na(d))))
  })

  # ===========================================================================
  # NOMINAL
  # ===========================================================================
  tab_nom <- reactive({
    x <- datos()[[input$v_nom]]
    if (input$ordenar_nom) x <- factor(x, levels = names(sort(table(x), decreasing = TRUE)))
    frec_categorias(x, etiquetas[input$v_nom])
  })

  graf_nom <- reactive({
    df <- head(tab_nom(), -1)
    names(df)[1] <- "Cat"
    df$Cat <- factor(df$Cat, levels = df$Cat)
    titulo <- paste("Distribución de", etiquetas[input$v_nom])
    if (input$g_nom == "pastel") {
      ggplot(df, aes(x = "", y = fi, fill = Cat)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(fi, "\n(", round(`pi (%)`, 1), "%)")),
                  position = position_stack(vjust = 0.5), size = 4) +
        scale_fill_brewer(palette = "Set2", name = etiquetas[input$v_nom]) +
        labs(title = titulo) + theme_void(base_size = 13) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      g <- ggplot(df, aes(x = Cat, y = fi, fill = Cat)) +
        geom_col(color = "white", width = 0.7, show.legend = FALSE) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = titulo, x = etiquetas[input$v_nom], y = "Frecuencia absoluta (fi)") + tema
      if (input$g_nom == "barrash") {
        g + coord_flip() + geom_text(aes(label = fi), hjust = -0.3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      } else {
        g + geom_text(aes(label = fi), vjust = -0.5) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      }
    }
  })

  output$t_nom <- renderDT(tabla_dt(tab_nom()))
  output$p_nom <- renderPlot(graf_nom())
  output$i_nom <- renderUI({
    df <- head(tab_nom(), -1)
    mx <- df[which.max(df$fi), ]; mn <- df[which.min(df$fi), ]
    div(class = "interpretacion",
        strong("Lectura: "),
        sprintf("La categoría con mayor frecuencia (moda) es «%s», con %d casos (%.1f %%). La de menor frecuencia es «%s», con %d casos (%.1f %%). Al ser una variable nominal, no se calculan frecuencias acumuladas.",
                mx[[1]], mx$fi, mx$`pi (%)`, mn[[1]], mn$fi, mn$`pi (%)`))
  })
  output$dlt_nom <- dl_csv(tab_nom, function() paste0("tabla_", input$v_nom))
  output$dlg_nom <- dl_png(graf_nom, function() paste0("grafico_", input$v_nom))

  # ===========================================================================
  # ORDINAL
  # ===========================================================================
  tab_ord <- reactive(frec_categorias(datos()[[input$v_ord]], etiquetas[input$v_ord], acumuladas = TRUE))

  graf_ord <- reactive({
    df <- head(tab_ord(), -1)
    names(df)[1] <- "Cat"
    df$Cat <- factor(df$Cat, levels = df$Cat)
    titulo <- paste("Distribución de", etiquetas[input$v_ord])
    if (input$g_ord == "pastel") {
      ggplot(df, aes(x = "", y = fi, fill = Cat)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(fi, "\n(", round(`pi (%)`, 1), "%)")),
                  position = position_stack(vjust = 0.5), size = 4) +
        scale_fill_brewer(palette = "RdYlGn", name = etiquetas[input$v_ord]) +
        labs(title = titulo) + theme_void(base_size = 13) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    } else if (input$g_ord == "ojiva") {
      ggplot(df, aes(x = Cat, y = Fi, group = 1)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_point(color = "darkred", size = 3) +
        geom_text(aes(label = Fi), vjust = -1) +
        scale_y_continuous(limits = c(0, max(df$Fi) * 1.1)) +
        labs(title = paste("Ojiva de", etiquetas[input$v_ord]),
             x = etiquetas[input$v_ord], y = "Frecuencia absoluta acumulada (Fi)") + tema
    } else {
      ggplot(df, aes(x = Cat, y = fi, fill = Cat)) +
        geom_col(color = "white", width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = fi), vjust = -0.5) +
        scale_fill_brewer(palette = "RdYlGn") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(title = titulo, x = etiquetas[input$v_ord], y = "Frecuencia absoluta (fi)") + tema
    }
  })

  output$t_ord <- renderDT(tabla_dt(tab_ord()))
  output$p_ord <- renderPlot(graf_ord())
  output$i_ord <- renderUI({
    df <- head(tab_ord(), -1)
    mx <- df[which.max(df$fi), ]
    med <- df[which(df$Hi >= 0.5)[1], ]
    div(class = "interpretacion",
        strong("Lectura: "),
        sprintf("La categoría modal es «%s», con %d casos (%.1f %%). La categoría mediana, es decir, aquella en la que la frecuencia relativa acumulada alcanza el 50 %%, es «%s» (Hi = %.3f).",
                mx[[1]], mx$fi, mx$`pi (%)`, med[[1]], med$Hi))
  })
  output$dlt_ord <- dl_csv(tab_ord, function() paste0("tabla_", input$v_ord))
  output$dlg_ord <- dl_png(graf_ord, function() paste0("grafico_", input$v_ord))

  # ===========================================================================
  # DISCRETA
  # ===========================================================================
  tab_dis <- reactive({
    x <- datos()[[input$v_dis]]
    frec_categorias(factor(x, levels = sort(unique(x))), "xi", acumuladas = TRUE)
  })

  graf_dis <- reactive({
    x  <- datos()[[input$v_dis]]
    df <- head(tab_dis(), -1)
    df$xi <- as.numeric(df$xi)
    titulo <- paste("Distribución de", etiquetas[input$v_dis])
    switch(input$g_dis,
      bastones = ggplot(df, aes(x = xi, y = fi)) +
        geom_segment(aes(xend = xi, yend = 0), color = "steelblue", linewidth = 1.2) +
        geom_point(color = "darkred", size = 3) +
        geom_text(aes(label = fi), vjust = -1) +
        scale_x_continuous(breaks = df$xi) +
        scale_y_continuous(limits = c(0, max(df$fi) * 1.15)) +
        labs(title = paste("Diagrama de bastones de", etiquetas[input$v_dis]),
             x = etiquetas[input$v_dis], y = "Frecuencia absoluta (fi)") + tema,
      barras = ggplot(df, aes(x = factor(xi), y = fi)) +
        geom_col(fill = "steelblue", color = "white", width = 0.7) +
        geom_text(aes(label = fi), vjust = -0.5) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(title = titulo, x = etiquetas[input$v_dis], y = "Frecuencia absoluta (fi)") + tema,
      escalon = ggplot(rbind(data.frame(xi = min(df$xi) - 1, Fi = 0), df[, c("xi", "Fi")]),
                       aes(x = xi, y = Fi)) +
        geom_step(direction = "hv", color = "steelblue", linewidth = 1) +
        geom_point(data = df, color = "darkred", size = 2.5) +
        scale_x_continuous(breaks = df$xi) +
        labs(title = paste("Frecuencia acumulada de", etiquetas[input$v_dis]),
             x = etiquetas[input$v_dis], y = "Frecuencia absoluta acumulada (Fi)") + tema,
      caja = ggplot(data.frame(x = x), aes(x = x, y = "")) +
        geom_boxplot(fill = "lightblue", width = 0.4, outlier.color = "darkred") +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
        labs(title = paste("Diagrama de caja de", etiquetas[input$v_dis]),
             x = etiquetas[input$v_dis], y = NULL,
             caption = "El rombo rojo indica la media") + tema
    )
  })

  output$t_dis <- renderDT(tabla_dt(tab_dis()))
  output$m_dis <- renderDT(datatable(medidas(datos()[[input$v_dis]]), rownames = FALSE,
                                     class = "compact stripe",
                                     options = list(dom = "t", paging = FALSE, ordering = FALSE)))
  output$p_dis <- renderPlot(graf_dis())
  output$i_dis <- renderUI({
    x  <- datos()[[input$v_dis]]
    df <- head(tab_dis(), -1)
    mx <- df[which.max(df$fi), ]
    div(class = "interpretacion",
        strong("Lectura: "),
        sprintf("Los valores van de %s a %s. El valor más frecuente es %s, con %d casos (%.1f %%). La media es %.2f, la mediana %s y la desviación estándar %.2f (CV = %.1f %%).",
                min(x), max(x), mx$xi, mx$fi, mx$`pi (%)`, mean(x), median(x), sd(x),
                100 * sd(x) / mean(x)))
  })
  output$dlt_dis <- dl_csv(tab_dis, function() paste0("tabla_", input$v_dis))
  output$dlg_dis <- dl_png(graf_dis, function() paste0("grafico_", input$v_dis))

  # ===========================================================================
  # CONTINUA
  # ===========================================================================
  par_con <- reactive({
    x <- datos()[[input$v_con]]
    k <- if (input$sturges) NULL else input$k_con
    param_continua(x, decimales[[input$v_con]], k)
  })

  tab_con <- reactive({
    frec_continua(datos()[[input$v_con]], par_con(), decimales[[input$v_con]],
                  derecha = input$cierre == "der")
  })

  graf_con <- reactive({
    x  <- datos()[[input$v_con]]
    p  <- par_con()
    df <- head(tab_con(), -1)
    et <- etiquetas[input$v_con]
    cerrado <- if (input$cierre == "der") "right" else "left"
    rotar <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

    switch(input$g_con,
      hist = ggplot(data.frame(x = x), aes(x = x)) +
        geom_histogram(breaks = p$cortes, closed = cerrado,
                       fill = "steelblue", color = "white") +
        stat_bin(breaks = p$cortes, closed = cerrado, geom = "text",
                 aes(label = after_stat(count)), vjust = -0.5) +
        scale_x_continuous(breaks = p$cortes) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(title = paste("Histograma de", et), x = et, y = "Frecuencia absoluta (fi)") +
        tema + rotar,
      poligono = {
        pts <- data.frame(mi = c(df$mi[1] - p$A, df$mi, df$mi[nrow(df)] + p$A),
                          fi = c(0, df$fi, 0))
        ggplot() +
          geom_histogram(data = data.frame(x = x), aes(x = x), breaks = p$cortes,
                         closed = cerrado, fill = "lightblue", color = "white", alpha = 0.6) +
          geom_line(data = pts, aes(x = mi, y = fi), color = "darkred", linewidth = 1) +
          geom_point(data = pts, aes(x = mi, y = fi), color = "darkred", size = 2.5) +
          scale_x_continuous(breaks = round(pts$mi, decimales[[input$v_con]] + 2)) +
          labs(title = paste("Polígono de frecuencias de", et), x = paste(et, "(marca de clase)"),
               y = "Frecuencia absoluta (fi)") + tema + rotar
      },
      ojiva = ggplot(data.frame(limite = p$cortes, Fi = c(0, df$Fi)), aes(x = limite, y = Fi)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_point(color = "darkred", size = 2.5) +
        geom_text(aes(label = Fi), vjust = -0.8, size = 3.5) +
        scale_x_continuous(breaks = p$cortes) +
        scale_y_continuous(limits = c(0, p$n * 1.08)) +
        labs(title = paste("Ojiva de", et), x = paste(et, "(límite superior de clase)"),
             y = "Frecuencia absoluta acumulada (Fi)") + tema + rotar,
      densidad = ggplot(data.frame(x = x), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), breaks = p$cortes, closed = cerrado,
                       fill = "steelblue", color = "white", alpha = 0.6) +
        geom_density(color = "darkred", linewidth = 1.2) +
        geom_vline(xintercept = mean(x), linetype = "dashed") +
        annotate("text", x = mean(x), y = 0, hjust = -0.1, vjust = -0.5,
                 label = paste("Media =", round(mean(x), decimales[[input$v_con]] + 2))) +
        scale_x_continuous(breaks = p$cortes) +
        labs(title = paste("Densidad de", et), x = et, y = "Densidad") + tema + rotar,
      caja = ggplot(data.frame(x = x), aes(x = x, y = "")) +
        geom_boxplot(fill = "lightblue", width = 0.4, outlier.color = "darkred") +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
        labs(title = paste("Diagrama de caja de", et), x = et, y = NULL,
             caption = "El rombo rojo indica la media") + tema
    )
  })

  output$par_con <- renderDT({
    p <- par_con()
    datatable(data.frame(
      Parámetro = c("Mínimo", "Máximo", "Rango (R)", "Número de intervalos (k)",
                    "Amplitud (A)", "Rango corregido (R')", "Diferencia (R' - R)",
                    "Mínimo corregido", "Máximo corregido"),
      Valor = c(p$Min, p$Max, p$R, p$k, p$A, p$Rc, p$Dif, p$Xmin, p$Xmax)),
      rownames = FALSE, class = "compact stripe",
      options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  output$m_con <- renderDT(datatable(medidas(datos()[[input$v_con]], con_moda = FALSE),
                                     rownames = FALSE, class = "compact stripe",
                                     options = list(dom = "t", paging = FALSE, ordering = FALSE)))
  output$t_con <- renderDT(tabla_dt(tab_con()))
  output$p_con <- renderPlot(graf_con())
  output$i_con <- renderUI({
    x  <- datos()[[input$v_con]]
    df <- head(tab_con(), -1)
    mx <- df[which.max(df$fi), ]
    cm <- df[which(df$Fi >= length(x) / 2)[1], ]
    div(class = "interpretacion",
        strong("Lectura: "),
        sprintf("Los datos se agruparon en %d intervalos de amplitud %s. La clase modal es %s, con %d casos (%.1f %%). La clase mediana es %s. La media es %.3f, la mediana %.3f y la desviación estándar %.3f (CV = %.1f %%).",
                par_con()$k, par_con()$A, mx$Intervalo, mx$fi, 100 * mx$hi, cm$Intervalo,
                mean(x), median(x), sd(x), 100 * sd(x) / mean(x)))
  })
  output$dlt_con <- dl_csv(tab_con, function() paste0("tabla_", input$v_con))
  output$dlg_con <- dl_png(graf_con, function() paste0("grafico_", input$v_con))

  # ===========================================================================
  # CRUCE DE DOS VARIABLES CUALITATIVAS
  # ===========================================================================
  tabla_cruce <- reactive({
    validate(need(input$v_fila != input$v_col, "Seleccione dos variables diferentes."))
    d <- datos()
    table(d[[input$v_fila]], d[[input$v_col]])
  })

  tab_cruce <- reactive({
    t <- tabla_cruce()
    m <- switch(input$tipo_cruce,
      abs  = addmargins(t),
      conj = addmargins(prop.table(t)),
      fila = addmargins(prop.table(t, 1), 2),
      col  = addmargins(prop.table(t, 2), 1))
    df <- as.data.frame.matrix(m)
    if (input$tipo_cruce != "abs") df <- round(df * ifelse(input$pct_cruce, 100, 1),
                                               ifelse(input$pct_cruce, 2, 4))
    names(df)[names(df) == "Sum"] <- "Total"
    rn <- rownames(df); rn[rn == "Sum"] <- "Total"
    df <- cbind(setNames(data.frame(rn, stringsAsFactors = FALSE),
                         paste(etiquetas[input$v_fila], "/", etiquetas[input$v_col])), df)
    rownames(df) <- NULL
    df
  })

  graf_cruce <- reactive({
    df <- as.data.frame(tabla_cruce())
    names(df) <- c("Fila", "Columna", "Freq")
    ef <- etiquetas[input$v_fila]; ec <- etiquetas[input$v_col]
    pal <- paleta_cual(input$v_col)
    base <- labs(fill = ec)
    titulo <- paste(ef, "vs", ec)

    switch(input$g_cruce,
      agrupadas = ggplot(df, aes(x = Fila, y = Freq, fill = Columna)) +
        geom_col(position = position_dodge(width = 0.9), color = "white") +
        geom_text(aes(label = Freq), position = position_dodge(width = 0.9),
                  vjust = -0.4, size = 3.2) +
        scale_fill_brewer(palette = pal) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(title = paste("Barras conjuntas:", titulo), x = ef,
             y = "Frecuencia absoluta (fi)") + base + tema,
      apiladas = ggplot(df, aes(x = Fila, y = Freq, fill = Columna)) +
        geom_col(position = position_stack(reverse = TRUE), color = "white", width = 0.7) +
        geom_text(aes(label = Freq), position = position_stack(vjust = 0.5, reverse = TRUE),
                  size = 3.5) +
        scale_fill_brewer(palette = pal) +
        labs(title = paste("Barras apiladas:", titulo), x = ef,
             y = "Frecuencia absoluta (fi)") + base + tema,
      apiladash = ggplot(df, aes(x = Freq, y = Fila, fill = Columna)) +
        geom_col(position = position_stack(reverse = TRUE), color = "white", width = 0.7) +
        geom_text(aes(label = Freq), position = position_stack(vjust = 0.5, reverse = TRUE),
                  size = 3.5) +
        scale_fill_brewer(palette = pal) +
        guides(fill = guide_legend(nrow = 1)) +
        labs(title = paste("Barras apiladas horizontales:", titulo), x = "Frecuencia absoluta (fi)",
             y = ef) + base + tema,
      cien = {
        df <- df %>% group_by(Fila) %>% mutate(pct = Freq / sum(Freq)) %>% ungroup()
        ggplot(df, aes(x = Fila, y = Freq, fill = Columna)) +
          geom_col(position = position_fill(reverse = TRUE), color = "white", width = 0.7) +
          geom_text(aes(label = paste0(round(100 * pct, 1), "%")),
                    position = position_fill(vjust = 0.5, reverse = TRUE), size = 3.3) +
          scale_fill_brewer(palette = pal) +
          scale_y_continuous(labels = scales::percent) +
          labs(title = paste("Barras apiladas al 100 %:", titulo), x = ef,
               y = "Porcentaje dentro de cada categoría") + base + tema
      }
    )
  })

  output$t_cruce <- renderDT(tabla_dt(tab_cruce()))
  output$p_cruce <- renderPlot(graf_cruce())
  output$chi_cruce <- renderUI({
    t  <- tabla_cruce()
    pr <- suppressWarnings(chisq.test(t))
    n  <- sum(t)
    V  <- sqrt(as.numeric(pr$statistic) / (n * (min(dim(t)) - 1)))
    menores5 <- sum(pr$expected < 5)
    pct5 <- 100 * menores5 / length(pr$expected)
    fuerza <- if (V < 0.1) "muy débil" else if (V < 0.3) "débil" else if (V < 0.5) "moderada" else "fuerte"
    conclusion <- if (pr$p.value < 0.05) {
      "Se rechaza la hipótesis de independencia: existe asociación estadísticamente significativa entre las variables (α = 0.05)."
    } else {
      "No se rechaza la hipótesis de independencia: no hay evidencia de asociación estadísticamente significativa entre las variables (α = 0.05)."
    }
    div(class = "interpretacion",
        p(sprintf("χ² = %.3f;  gl = %d;  p = %.4f;  V de Cramér = %.3f (asociación %s).",
                  pr$statistic, pr$parameter, pr$p.value, V, fuerza)),
        p(conclusion),
        p(em(sprintf("Frecuencias esperadas menores a 5: %d de %d celdas (%.1f %%). %s",
                     menores5, length(pr$expected), pct5,
                     if (pct5 > 20) "Más del 20 % de las celdas incumple el supuesto; interprete la prueba con cautela."
                     else "Se cumple el supuesto de la prueba."))))
  })
  output$dlt_cruce <- dl_csv(tab_cruce, function() paste0("contingencia_", input$v_fila, "_", input$v_col))
  output$dlg_cruce <- dl_png(graf_cruce, function() paste0("grafico_", input$v_fila, "_", input$v_col))
}

shinyApp(ui, server)
