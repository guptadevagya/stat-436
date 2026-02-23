library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(DT)
library(bslib)

# ============================================
# data load + prep
# ============================================

data_url <- "https://raw.githubusercontent.com/guptadevagya/stat-436/main/hw2/Heart_Disease_Mortality.csv"

raw_df <- read_csv(data_url, show_col_types = FALSE) %>%
  mutate(Data_Value = suppressWarnings(as.numeric(Data_Value)))

heart_df <- raw_df %>%
  filter(
    Topic == "Heart Disease Mortality",
    Year %in% c(2019, "2019"),
    GeographicLevel == "State"
  )

# sex summary (overall / male / female)
sex_long <- heart_df %>%
  filter(
    StratificationCategory1 == "Sex",
    Stratification1 %in% c("Overall", "Male", "Female"),
    Stratification2 == "Overall"
  ) %>%
  transmute(
    state_name = LocationDesc,
    state_abbr = LocationAbbr,
    sex_group = Stratification1,
    rate = Data_Value
  ) %>%
  group_by(state_name, state_abbr, sex_group) %>%
  summarize(rate = mean(rate, na.rm = TRUE), .groups = "drop")

sex_wide <- sex_long %>%
  pivot_wider(names_from = sex_group, values_from = rate) %>%
  rename(
    overall_rate = Overall,
    male_rate = Male,
    female_rate = Female
  ) %>%
  mutate(gender_gap = male_rate - female_rate)

# racial disparity summary (max - min within each state)
race_long <- heart_df %>%
  filter(
    Stratification1 == "Overall",
    Stratification2 != "Overall"
  ) %>%
  transmute(
    state_name = LocationDesc,
    state_abbr = LocationAbbr,
    race_group = Stratification2,
    race_rate = Data_Value
  )

race_summary <- race_long %>%
  group_by(state_name, state_abbr) %>%
  summarize(
    race_groups_available = sum(!is.na(race_rate)),
    highest_race_rate = ifelse(all(is.na(race_rate)), NA_real_, max(race_rate, na.rm = TRUE)),
    lowest_race_rate  = ifelse(all(is.na(race_rate)), NA_real_, min(race_rate, na.rm = TRUE)),
    racial_gap = highest_race_rate - lowest_race_rate,
    .groups = "drop"
  )

app_df <- sex_wide %>%
  full_join(race_summary, by = c("state_name", "state_abbr")) %>%
  mutate(
    geography_type = case_when(
      state_abbr == "DC" ~ "District of Columbia",
      state_abbr %in% state.abb ~ "State",
      TRUE ~ "Territory"
    ),
    map_supported = state_abbr %in% c(state.abb, "DC")
  ) %>%
  arrange(state_name)

# ============================================
# helpers
# ============================================

metric_meta <- function(metric_key) {
  switch(
    metric_key,
    overall_rate = list(
      col = "overall_rate",
      label = "Overall Mortality Rate",
      legend = "Deaths per 100k",
      desc = "Age-adjusted heart disease mortality rate (adults age 35+)."
    ),
    gender_gap = list(
      col = "gender_gap",
      label = "Male - Female Mortality Gap",
      legend = "Rate difference",
      desc = "Difference in mortality rate between men and women (male minus female)."
    ),
    racial_gap = list(
      col = "racial_gap",
      label = "Racial Disparity Gap",
      legend = "Rate difference",
      desc = "Gap between the highest and lowest reported racial/ethnic group rates within a state."
    )
  )
}

fmt1 <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f", x))

# ============================================
# ui
# ============================================

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins"),
    primary = "#2563EB",
    bg = "#F8FAFC",
    fg = "#0F172A"
  ),
  
  tags$head(
    tags$style(HTML("
      body { background: #f8fafc; }

      .container-fluid {
        max-width: 1680px;
        padding-top: 14px;
        padding-bottom: 20px;
      }

      .title-panel h2, .title-panel h1 {
        font-weight: 700;
        letter-spacing: -0.2px;
        margin-bottom: 8px;
        color: #0f172a;
      }

      .well {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 16px;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.05);
        padding: 18px 18px 14px 18px;
      }

      .well .form-group { margin-bottom: 16px; }
      .well .help-block {
        color: #475569;
        font-size: 12.5px;
        line-height: 1.45;
        margin-top: 8px;
      }
      .well hr {
        margin-top: 14px;
        margin-bottom: 14px;
        border-top: 1px solid #e5e7eb;
      }

      .metric-card {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 14px 16px;
        margin-bottom: 14px;
        box-shadow: 0 4px 12px rgba(15, 23, 42, 0.04);
      }

      .metric-card .metric-title {
        font-weight: 700;
        font-size: 17px;
        color: #0f172a;
        margin-bottom: 4px;
      }

      .metric-card .metric-desc {
        color: #475569;
        font-size: 13px;
        line-height: 1.45;
      }

      .nav-tabs {
        border-bottom: 1px solid #e5e7eb;
        margin-bottom: 12px;
      }

      .nav-tabs > li > a {
        color: #334155;
        font-weight: 600;
        border-radius: 10px 10px 0 0;
        margin-right: 6px;
        border: 1px solid transparent;
      }

      .nav-tabs > li > a:hover {
        background: #eef2ff;
        color: #1d4ed8;
        border-color: transparent;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #1d4ed8;
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-bottom-color: #ffffff;
      }

      .tab-content {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 0 14px 14px 14px;
        padding: 12px;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.04);
      }

      .map-note {
        color: #64748b;
        font-size: 12.5px;
        margin-top: 6px;
      }

      .plotly.html-widget,
      .dataTables_wrapper {
        margin-top: 4px;
      }

      .irs-from, .irs-to, .irs-single { background: #2563EB; }
      .irs-bar {
        background: #2563EB;
        border-top: 1px solid #2563EB;
        border-bottom: 1px solid #2563EB;
      }

      table.dataTable thead th {
        background-color: #f8fafc;
        color: #0f172a;
        font-weight: 600;
        border-bottom: 1px solid #e5e7eb !important;
        white-space: nowrap;
      }

      table.dataTable tbody td {
        white-space: nowrap;
      }

      table.dataTable tbody tr:hover {
        background-color: #f8fbff !important;
      }
    "))
  ),
  
  div(class = "title-panel",
      titlePanel("U.S. Heart Disease Mortality by State (2018–2020 Average)")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4(style = "margin-top: 0; font-weight: 700;", "Explore the data"),
      
      radioButtons(
        "metric",
        "Metric",
        choices = c(
          "Overall mortality" = "overall_rate",
          "Gender difference (male - female)" = "gender_gap",
          "Racial disparity (max - min)" = "racial_gap"
        ),
        selected = "overall_rate"
      ),
      
      radioButtons(
        "rank_order",
        "Ranked bars order",
        choices = c("Highest values" = "desc", "Lowest values" = "asc"),
        selected = "desc",
        inline = TRUE
      ),
      
      sliderInput(
        "top_n",
        "Number of places to show in Ranked Bars",
        min = 5, max = 20, value = 10, step = 1
      ),
      
      checkboxInput(
        "include_territories",
        "Include territories in bars/table (e.g., Guam, Puerto Rico)",
        value = TRUE
      ),
      
      hr(),
      
      helpText("Use the tabs to switch between Map, Ranked Bars, and Table. Territories may appear in the bars/table, but the map shows states and DC only.")
    ),
    
    mainPanel(
      width = 9,
      
      div(
        class = "metric-card",
        div(class = "metric-title", textOutput("metric_title", inline = TRUE)),
        div(class = "metric-desc", textOutput("metric_desc", inline = TRUE))
      ),
      
      tabsetPanel(
        id = "view_mode",
        
        tabPanel(
          "Map",
          plotlyOutput("map_plot", height = "560px"),
          div(class = "map-note", textOutput("map_note", inline = TRUE))
        ),
        
        tabPanel(
          "Ranked Bars",
          plotlyOutput("bar_plot", height = "560px")
        ),
        
        tabPanel(
          "Table",
          div(style = "width: 100%; overflow-x: auto;", DTOutput("state_table"))
        )
      )
    )
  )
)

# ============================================
# server
# ============================================

server <- function(input, output, session) {
  
  current_metric <- reactive({
    metric_meta(input$metric)
  })
  
  filtered_geos <- reactive({
    if (isTRUE(input$include_territories)) {
      app_df
    } else {
      app_df %>% filter(geography_type != "Territory")
    }
  })
  
  metric_ready <- reactive({
    info <- current_metric()
    filtered_geos() %>%
      mutate(metric_value = .data[[info$col]])
  })
  
  ranked_rows <- reactive({
    x <- metric_ready() %>% filter(!is.na(metric_value))
    x <- if (identical(input$rank_order, "desc")) {
      arrange(x, desc(metric_value))
    } else {
      arrange(x, metric_value)
    }
    slice_head(x, n = input$top_n)
  })
  
  output$metric_title <- renderText(current_metric()$label)
  output$metric_desc  <- renderText(current_metric()$desc)
  
  output$map_note <- renderText({
    if (isTRUE(input$include_territories)) {
      "Note: Territories remain available in Ranked Bars and Table, but the map only displays states/DC."
    } else {
      ""
    }
  })
  
  output$map_plot <- renderPlotly({
    info <- current_metric()
    
    map_data <- metric_ready() %>%
      filter(map_supported) %>%
      mutate(
        hover_txt = paste0(
          "<b>", state_name, " (", state_abbr, ")</b>",
          "<br>", info$label, ": ", fmt1(metric_value),
          "<br>Overall: ", fmt1(overall_rate),
          "<br>Male: ", fmt1(male_rate),
          "<br>Female: ", fmt1(female_rate),
          "<br>Male - Female Gap: ", fmt1(gender_gap),
          "<br>Racial Gap: ", fmt1(racial_gap)
        )
      )
    
    validate(
      need(nrow(map_data) > 0, "No map data available."),
      need(sum(!is.na(map_data$metric_value)) > 0, "No values available for the selected metric.")
    )
    
    plot_ly(
      data = map_data,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~state_abbr,
      z = ~metric_value,
      text = ~hover_txt,
      hoverinfo = "text",
      colors = c("#0EA5E9", "#FDE68A", "#EF4444"),
      colorbar = list(title = info$legend)
    ) %>%
      layout(
        title = list(text = paste(info$label, "Across U.S. States"), x = 0.02),
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE
        ),
        margin = list(l = 0, r = 0, t = 55, b = 0)
      )
  })
  
  output$bar_plot <- renderPlotly({
    info <- current_metric()
    
    bars <- ranked_rows() %>%
      mutate(
        hover_txt = paste0(
          "<b>", state_name, " (", state_abbr, ")</b>",
          "<br>", info$label, ": ", fmt1(metric_value),
          "<br>Overall: ", fmt1(overall_rate),
          "<br>Male: ", fmt1(male_rate),
          "<br>Female: ", fmt1(female_rate),
          "<br>Male - Female Gap: ", fmt1(gender_gap),
          "<br>Racial Gap: ", fmt1(racial_gap),
          "<br>Type: ", geography_type
        )
      )
    
    validate(need(nrow(bars) > 0, "No rows available for the ranked chart."))
    
    # reorder so highest appears at top after coord_flip
    bars <- if (identical(input$rank_order, "desc")) {
      arrange(bars, metric_value)
    } else {
      arrange(bars, desc(metric_value))
    }
    
    bars <- bars %>%
      mutate(state_label = factor(state_name, levels = state_name))
    
    p <- ggplot(bars, aes(x = state_label, y = metric_value, text = hover_txt)) +
      geom_col(fill = "#2563EB") +
      coord_flip() +
      labs(
        title = paste0(
          ifelse(input$rank_order == "desc", "Highest ", "Lowest "),
          input$top_n, " ", info$label, " Values"
        ),
        x = NULL,
        y = info$legend
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
        axis.text.y = element_text(size = 11, color = "#1f2937"),
        axis.text.x = element_text(color = "#334155"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 8)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 140, r = 20, t = 55, b = 50))
  })
  
  output$state_table <- renderDT({
    table_data <- metric_ready() %>%
      transmute(
        State = state_name,
        Abbr = state_abbr,
        Geography = geography_type,
        `Overall Rate` = round(overall_rate, 1),
        `Male Rate` = round(male_rate, 1),
        `Female Rate` = round(female_rate, 1),
        `Male - Female Gap` = round(gender_gap, 1),
        `Racial Gap` = round(racial_gap, 1),
        `Race Groups Available` = race_groups_available
      )
    
    datatable(
      table_data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
}

# ============================================
# run app
# ============================================

shinyApp(ui, server)