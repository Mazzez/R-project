# ============================================================================
# R SHINY APP - ANALYSE INTERACTIVE DES CONCENTRATIONS CO₂ GLOBALES
# ============================================================================
# Application web interactive pour explorer les données NOAA CO₂ (1979-2025)
# ============================================================================

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(scales)

# ============================================================================
# 1. PRÉPARATION DES DONNÉES
# ============================================================================

# Charger et nettoyer les données
co2_raw <- read.table("co2_mm_gl.csv",
                      header = TRUE,
                      sep = ",",
                      comment.char = "#",
                      stringsAsFactors = FALSE,
                      na.strings = c("", "NA", "-99.99"))

co2_clean <- co2_raw %>%
  mutate(
    Date = make_date(year, month, 15),
    Date_decimal = decimal,
    CO2_ppm = average,
    CO2_uncertainty = average_unc,
    CO2_trend = trend,
    Trend_uncertainty = trend_unc,
    ID = row_number(),
    Year = year,
    Month = month,
    Quarter = ceiling(month / 3),
    Season = case_when(
      month %in% c(12, 1, 2) ~ "Hiver",
      month %in% c(3, 4, 5) ~ "Printemps",
      month %in% c(6, 7, 8) ~ "Été",
      month %in% c(9, 10, 11) ~ "Automne"
    ),
    Decade = floor(Year / 10) * 10,
    Period = case_when(
      Year < 1990 ~ "1979-1989",
      Year >= 1990 & Year < 2000 ~ "1990-1999",
      Year >= 2000 & Year < 2010 ~ "2000-2009",
      Year >= 2010 & Year < 2020 ~ "2010-2019",
      Year >= 2020 ~ "2020-2025"
    ),
    CO2_anomaly = CO2_ppm - mean(CO2_ppm, na.rm = TRUE),
    Year_fraction = Year + (Month - 0.5) / 12
  ) %>%
  group_by(Year) %>%
  mutate(
    CO2_ppm = ifelse(is.na(CO2_ppm), mean(CO2_ppm, na.rm = TRUE), CO2_ppm)
  ) %>%
  ungroup() %>%
  select(ID, Date, Year, Month, Quarter, Season, Decade, Period,
         CO2_ppm, CO2_uncertainty, CO2_trend, Trend_uncertainty,
         CO2_anomaly, Date_decimal, Year_fraction) %>%
  arrange(Date)

# Statistiques calculées
general_stats <- summary(co2_clean$CO2_ppm)
lm_model <- lm(CO2_ppm ~ Year_fraction, data = co2_clean)
lm_summary <- summary(lm_model)

# ============================================================================
# 2. INTERFACE UTILISATEUR (UI)
# ============================================================================

ui <- navbarPage(
  title = "Analyse CO₂ Global NOAA (1979-2025)",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  # ========================================================================
  # TAB 1 : TABLEAU DE BORD
  # ========================================================================
  tabPanel("📊 Tableau de Bord",
           sidebarLayout(
             sidebarPanel(
               h3("🔧 Filtres", style = "color: #2c3e50; font-weight: bold;"),
               hr(),
               
               # Sélecteur de période
               sliderInput("year_range", 
                           "Période analysée:",
                           min = 1979, max = 2025, value = c(1979, 2025),
                           step = 1, sep = ""),
               
               # Sélecteur de saison
               checkboxGroupInput("seasons",
                                  "Saisons:",
                                  choices = c("Hiver", "Printemps", "Été", "Automne"),
                                  selected = c("Hiver", "Printemps", "Été", "Automne"),
                                  inline = FALSE),
               
               hr(),
               
               # Bouton de téléchargement
               downloadButton("download_data", "📥 Télécharger les données", 
                              class = "btn-primary btn-block"),
               
               br(), br(),
               
               # Informations
               h4("ℹ️ Informations", style = "color: #34495e;"),
               p("Cette application explore l'évolution du CO₂ global",
                 "basée sur les données du NOAA Global Monitoring Laboratory"),
               tags$ul(
                 tags$li("Données: 1979-2025"),
                 tags$li("Fréquence: Mensuelle"),
                 tags$li("Observations: 561 mois")
               )
             ),
             
             mainPanel(
               h2("📈 Vue d'ensemble des données CO₂", style = "color: #2c3e50; font-weight: bold;"),
               hr(),
               
               # Affichage des métriques clés
               fluidRow(
                 column(3,
                        div(class = "bg-light p-3 rounded",
                            h5("Concentration actuelle", class = "text-muted"),
                            h3(textOutput("current_co2"), class = "text-danger")
                        )
                 ),
                 column(3,
                        div(class = "bg-light p-3 rounded",
                            h5("Moyenne 1979-2025", class = "text-muted"),
                            h3(textOutput("avg_co2"), class = "text-info")
                        )
                 ),
                 column(3,
                        div(class = "bg-light p-3 rounded",
                            h5("Augmentation totale", class = "text-muted"),
                            h3(textOutput("total_increase"), class = "text-warning")
                        )
                 ),
                 column(3,
                        div(class = "bg-light p-3 rounded",
                            h5("Pente (ppm/an)", class = "text-muted"),
                            h3(textOutput("slope"), class = "text-success")
                        )
                 )
               ),
               
               br(),
               
               # Graphique interactif principal
               h4("Evolution temporelle du CO₂", style = "font-weight: bold;"),
               plotlyOutput("plot_evolution", height = "500px"),
               
               br(),
               
               # Tableau de statistiques
               h4("Statistiques descriptives", style = "font-weight: bold;"),
               DTOutput("stats_table")
             )
           )
  ),
  
  # ========================================================================
  # TAB 2 : VISUALISATIONS
  # ========================================================================
  tabPanel("📉 Visualisations",
           tabsetPanel(
             tabPanel("Cycle Saisonnier",
                      sidebarPanel(
                        h4("Options du graphique"),
                        checkboxInput("show_ci", "Afficher intervalle de confiance", value = TRUE),
                        selectInput("decade_filter", "Filtre par décennie:",
                                    choices = c("Toutes", sort(unique(co2_clean$Decade))))
                      ),
                      mainPanel(
                        plotlyOutput("plot_seasonal", height = "600px")
                      )
             ),
             
             tabPanel("Évolution par Décennie",
                      plotlyOutput("plot_decade_evolution", height = "600px")
             ),
             
             tabPanel("Distribution (Boxplot)",
                      plotlyOutput("plot_boxplot", height = "600px")
             ),
             
             tabPanel("Anomalies",
                      plotlyOutput("plot_anomalies", height = "600px")
             ),
             
             tabPanel("Croissance Annuelle",
                      plotlyOutput("plot_growth", height = "600px")
             )
           )
  ),
  
  # ========================================================================
  # TAB 3 : STATISTIQUES DÉTAILLÉES
  # ========================================================================
  tabPanel("📋 Statistiques",
           tabsetPanel(
             tabPanel("Par Année",
                      h3("Statistiques Annuelles du CO₂"),
                      DTOutput("stats_annual")
             ),
             
             tabPanel("Par Décennie",
                      h3("Statistiques par Décennie"),
                      DTOutput("stats_decade")
             ),
             
             tabPanel("Par Saison",
                      h3("Statistiques Saisonnières"),
                      DTOutput("stats_season")
             ),
             
             tabPanel("Données Brutes",
                      h3("Données Complètes (561 observations)"),
                      DTOutput("data_raw")
             )
           )
  ),
  
  # ========================================================================
  # TAB 4 : ANALYSE APPROFONDIE
  # ========================================================================
  tabPanel("🔬 Analyse Approfondie",
           fluidRow(
             column(6,
                    h3("Régression Linéaire", style = "font-weight: bold;"),
                    verbatimTextOutput("regression_summary")
             ),
             column(6,
                    h3("Analyse de Tendance", style = "font-weight: bold;"),
                    tableOutput("trend_analysis")
             )
           ),
           
           hr(),
           
           fluidRow(
             column(12,
                    h3("Sélecteur d'Analyse Personnalisée", style = "font-weight: bold;"),
                    fluidRow(
                      column(4,
                             selectInput("select_metric",
                                         "Métrique à analyser:",
                                         choices = c("CO2_ppm", "CO2_trend", "CO2_anomaly"),
                                         selected = "CO2_ppm")
                      ),
                      column(4,
                             selectInput("select_groupby",
                                         "Grouper par:",
                                         choices = c("Year", "Month", "Season", "Decade"),
                                         selected = "Year")
                      ),
                      column(4,
                             actionButton("analyze_btn", "Analyser", class = "btn-primary")
                      )
                    ),
                    br(),
                    plotlyOutput("custom_analysis", height = "500px")
             )
           )
  ),
  
  # ========================================================================
  # TAB 5 : À PROPOS
  # ========================================================================
  tabPanel("ℹ️ À Propos",
           fluidPage(
             h2("À Propos de cette Application", style = "color: #2c3e50; font-weight: bold;"),
             hr(),
             
             h3("Source des Données"),
             p("Les données proviennent du ",
               tags$strong("NOAA Global Monitoring Laboratory"),
               " - une institution scientifique de référence pour le monitoring du CO₂ global."),
             
             h3("Période d'Analyse"),
             p("Données mensuelles de janvier 1979 à décembre 2025 (561 observations)"),
             
             h3("Variables Disponibles"),
             tags$ul(
               tags$li("CO2_ppm: Concentration moyenne mensuelle de CO₂ en ppm"),
               tags$li("CO2_trend: Tendance désaisonnalisée"),
               tags$li("CO2_anomaly: Écart par rapport à la moyenne globale"),
               tags$li("CO2_uncertainty: Incertitude de mesure")
             ),
             
             h3("Résultats Clés"),
             tags$ul(
               tags$li("Augmentation totale: 88.6 ppm (1979-2025)"),
               tags$li("Pente: 1.88 ppm/an"),
               tags$li("R²: 0.985 (excellent ajustement)"),
               tags$li("Amplitude saisonnière: ~2.8 ppm")
             ),
             
             h3("Technologies"),
             p("Application développée avec R Shiny, ggplot2, plotly, et tidyverse"),
             
             hr(),
             p("📧 ", em("Pour toute question sur les données, contactez le NOAA GML")),
             p("🔗 ", tags$a("https://gml.noaa.gov/ccgg/trends/", 
                             href = "https://gml.noaa.gov/ccgg/trends/"))
           )
  )
)

# ============================================================================
# 3. LOGIQUE SERVEUR
# ============================================================================

server <- function(input, output, session) {
  
  # Données filtrées réactives
  filtered_data <- reactive({
    co2_clean %>%
      filter(Year >= input$year_range[1],
             Year <= input$year_range[2],
             Season %in% input$seasons)
  })
  
  # ========================================================================
  # MÉTRIQUES CLÉS
  # ========================================================================
  
  output$current_co2 <- renderText({
    latest <- co2_clean %>% 
      filter(Year == max(Year)) %>% 
      pull(CO2_ppm) %>% 
      mean(na.rm = TRUE)
    paste0(round(latest, 1), " ppm")
  })
  
  output$avg_co2 <- renderText({
    avg <- mean(co2_clean$CO2_ppm, na.rm = TRUE)
    paste0(round(avg, 1), " ppm")
  })
  
  output$total_increase <- renderText({
    co2_1979 <- mean(co2_clean$CO2_ppm[co2_clean$Year == 1979], na.rm = TRUE)
    co2_2025 <- mean(co2_clean$CO2_ppm[co2_clean$Year == 2025], na.rm = TRUE)
    increase <- co2_2025 - co2_1979
    paste0("+", round(increase, 1), " ppm")
  })
  
  output$slope <- renderText({
    slope <- round(coef(lm_model)[2], 2)
    paste0(slope, " ppm/an")
  })
  
  # ========================================================================
  # GRAPHIQUE PRINCIPAL
  # ========================================================================
  
  output$plot_evolution <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = CO2_ppm)) +
      geom_line(color = "#E41A1C", linewidth = 0.8, alpha = 0.8) +
      geom_smooth(method = "loess", span = 0.1, color = "#377EB8",
                  linetype = "dashed", se = TRUE, alpha = 0.2, linewidth = 0.8) +
      geom_smooth(method = "lm", color = "#4DAF4A", se = FALSE, linewidth = 0.6) +
      labs(
        title = "Évolution des concentrations CO₂ globales",
        x = "Année",
        y = "CO₂ (ppm)",
        caption = paste("Pente:", round(coef(lm_model)[2], 3), "ppm/an | R²:", round(lm_summary$r.squared, 3))
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14))
    
    ggplotly(p, tooltip = "x")
  })
  
  # ========================================================================
  # GRAPHIQUE SAISONNIER
  # ========================================================================
  
  output$plot_seasonal <- renderPlotly({
    data <- co2_clean
    if (input$decade_filter != "Toutes") {
      data <- data %>% filter(Decade == as.numeric(input$decade_filter))
    }
    
    monthly_avg <- data %>%
      group_by(Month) %>%
      summarise(CO2_mean = mean(CO2_ppm, na.rm = TRUE),
                CO2_sd = sd(CO2_ppm, na.rm = TRUE),
                .groups = 'drop')
    
    p <- ggplot(monthly_avg, aes(x = Month, y = CO2_mean)) +
      geom_line(color = "#984EA3", linewidth = 1.2) +
      geom_point(color = "#984EA3", size = 3) +
      {if(input$show_ci) geom_ribbon(aes(ymin = CO2_mean - CO2_sd, 
                                         ymax = CO2_mean + CO2_sd),
                                     fill = "#984EA3", alpha = 0.2)} +
      labs(
        title = "Cycle saisonnier du CO₂",
        x = "Mois",
        y = "CO₂ (ppm)"
      ) +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # ========================================================================
  # GRAPHIQUE BOXPLOT PAR DÉCENNIE
  # ========================================================================
  
  output$plot_boxplot <- renderPlotly({
    p <- ggplot(co2_clean, aes(x = factor(Decade), y = CO2_ppm, fill = factor(Decade))) +
      geom_boxplot(alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        title = "Distribution du CO₂ par décennie",
        x = "Décennie",
        y = "CO₂ (ppm)"
      ) +
      scale_fill_viridis_d(guide = "none") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14))
    
    ggplotly(p)
  })
  
  # ========================================================================
  # GRAPHIQUE ANOMALIES
  # ========================================================================
  
  output$plot_anomalies <- renderPlotly({
    co2_monthly_avg <- co2_clean %>%
      group_by(Month) %>%
      summarise(monthly_avg = mean(CO2_ppm, na.rm = TRUE))
    
    co2_anomalies <- co2_clean %>%
      left_join(co2_monthly_avg, by = "Month") %>%
      mutate(anomaly = CO2_ppm - monthly_avg)
    
    p <- ggplot(co2_anomalies, aes(x = Date, y = anomaly)) +
      geom_line(color = "#FF7F00", linewidth = 0.6, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_smooth(method = "loess", span = 0.2, color = "#A65628", se = TRUE, alpha = 0.2) +
      labs(
        title = "Anomalies du CO₂",
        x = "Année",
        y = "Anomalie (ppm)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14))
    
    ggplotly(p)
  })
  
  # ========================================================================
  # GRAPHIQUE CROISSANCE ANNUELLE
  # ========================================================================
  
  output$plot_growth <- renderPlotly({
    annual_growth <- co2_clean %>%
      group_by(Year) %>%
      summarise(mean_co2 = mean(CO2_ppm, na.rm = TRUE)) %>%
      mutate(growth = mean_co2 - lag(mean_co2)) %>%
      filter(!is.na(growth))
    
    p <- ggplot(annual_growth, aes(x = Year, y = growth)) +
      geom_col(fill = "#F781BF", alpha = 0.7) +
      geom_hline(yintercept = mean(annual_growth$growth, na.rm = TRUE),
                 linetype = "dashed", color = "#E41A1C") +
      geom_smooth(method = "loess", color = "#377EB8", se = TRUE, alpha = 0.2) +
      labs(
        title = "Augmentation annuelle du CO₂",
        x = "Année",
        y = "Augmentation (ppm/an)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14))
    
    ggplotly(p)
  })
  
  # ========================================================================
  # GRAPHIQUE ÉVOLUTION PAR DÉCENNIE
  # ========================================================================
  
  output$plot_decade_evolution <- renderPlotly({
    seasonal_by_decade <- co2_clean %>%
      group_by(Decade, Month) %>%
      summarise(CO2_mean = mean(CO2_ppm, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(Month_name = month.abb[Month],
             Decade_label = paste0(Decade, "s"))
    
    p <- ggplot(seasonal_by_decade, aes(x = Month_name, y = CO2_mean,
                                        color = Decade_label, group = Decade_label)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      labs(
        title = "Évolution du cycle saisonnier par décennie",
        x = "Mois",
        y = "CO₂ (ppm)",
        color = "Décennie"
      ) +
      scale_color_viridis_d() +
      scale_x_discrete(limits = month.abb) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # ========================================================================
  # TABLEAUX DE STATISTIQUES
  # ========================================================================
  
  output$stats_table <- renderDT({
    data.frame(
      "Statistique" = c("Min", "Q1", "Mèdiane", "Moyenne", "Q3", "Max", "écart-type"),
      "Valeur (ppm)" = c(
        round(general_stats[1], 2),
        round(general_stats[2], 2),
        round(general_stats[3], 2),
        round(general_stats[4], 2),
        round(general_stats[5], 2),
        round(general_stats[6], 2),
        round(sd(co2_clean$CO2_ppm), 2)
      )
    ) %>%
      datatable(options = list(pageLength = 5, dom = 'tip'))
  })
  
  output$stats_annual <- renderDT({
    annual_stats <- co2_clean %>% 
      group_by(Year) %>%
      summarise(
        "CO₂ moyen (ppm)" = round(mean(CO2_ppm, na.rm = TRUE), 2),
        "Min (ppm)" = round(min(CO2_ppm, na.rm = TRUE), 2),
        "Max (ppm)" = round(max(CO2_ppm, na.rm = TRUE), 2),
        "Amplitude" = round(max(CO2_ppm, na.rm = TRUE) - min(CO2_ppm, na.rm = TRUE), 2),
        .groups = 'drop'
      )
    
    datatable(annual_stats, options = list(pageLength = 10, 
                                           dom = 'ftiprB',
                                           buttons = c('csv', 'excel')),
              extensions = 'Buttons')
  })
  
  output$stats_decade <- renderDT({
    decade_stats <- co2_clean %>%
      group_by(Decade) %>%
      summarise(
        "Décennie" = paste0(Decade, "s"),
        "CO₂ moyen (ppm)" = round(mean(CO2_ppm, na.rm = TRUE), 2),
        "Min (ppm)" = round(min(CO2_ppm, na.rm = TRUE), 2),
        "Max (ppm)" = round(max(CO2_ppm, na.rm = TRUE), 2),
        "Augmentation (ppm)" = round(max(CO2_ppm, na.rm = TRUE) - min(CO2_ppm, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      select(-Decade)
    
    datatable(decade_stats, options = list(pageLength = 10, dom = 'ftiprB',
                                           buttons = c('csv', 'excel')),
              extensions = 'Buttons')
  })
  
  output$stats_season <- renderDT({
    season_stats <- co2_clean %>%
      group_by(Season) %>%
      summarise(
        "CO₂ moyen (ppm)" = round(mean(CO2_ppm, na.rm = TRUE), 2),
        "Écart-type"      = round(sd(CO2_ppm, na.rm = TRUE), 2),
        "Min (ppm)"       = round(min(CO2_ppm, na.rm = TRUE), 2),
        "Max (ppm)"       = round(max(CO2_ppm, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(Season)   # <-- remplacer l'ancien arrange sur le nom encodé
    
    datatable(
      season_stats,
      options   = list(pageLength = 10, dom = "ftiprB", buttons = c("csv", "excel")),
      extensions = "Buttons"
    )
  })
  
  output$data_raw <- renderDT({
    datatable(co2_clean,
              options = list(pageLength = 20,
                             dom = 'ftiprB',
                             buttons = c('csv', 'excel')),
              extensions = 'Buttons')
  })
  
  # ========================================================================
  # RÉSUMÉ DE RÉGRESSION
  # ========================================================================
  
  output$regression_summary <- renderPrint({
    print(lm_summary)
  })
  
  output$trend_analysis <- renderTable({
    data.frame(
      "Paramètre" = c("Pente", "Ordonnée à  l'origine", "R²", "Observations"),
      "Valeur" = c(
        paste0(round(coef(lm_model)[2], 4), " ppm/an"),
        paste0(round(coef(lm_model)[1], 2), " ppm"),
        round(lm_summary$r.squared, 4),
        nrow(co2_clean)
      )
    )
  })
  
  # ========================================================================
  # ANALYSE PERSONNALISÉE
  # ========================================================================
  
  observeEvent(input$analyze_btn, {
    output$custom_analysis <- renderPlotly({
      metric_col <- sym(input$select_metric)
      group_col <- sym(input$select_groupby)
      
      data_summary <- co2_clean %>%
        group_by(!!group_col) %>%
        summarise(Mean = mean(!!metric_col, na.rm = TRUE),
                  SD = sd(!!metric_col, na.rm = TRUE),
                  .groups = 'drop')
      
      p <- ggplot(data_summary, aes(x = !!group_col, y = Mean)) +
        geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                      width = 0.2, color = "black") +
        labs(
          title = paste("Analyse de", input$select_metric, "par", input$select_groupby),
          x = input$select_groupby,
          y = input$select_metric
        ) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
  })
  
  # ========================================================================
  # TÉLÉHARGEMENT DE DONNÉES
  # ========================================================================
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("CO2_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# ============================================================================
# 4. LANCER L'APPLICATION
# ============================================================================

shinyApp(ui, server)