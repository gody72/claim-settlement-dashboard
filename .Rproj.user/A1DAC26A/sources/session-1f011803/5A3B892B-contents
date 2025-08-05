library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(forcats)
library(bsicons)
library(shinyWidgets)
library(tidyr)

# Load dataset
claims <- read.csv("data/claims.csv")

safe_mean <- function(x) ifelse(all(is.na(x)), 0, mean(x, na.rm = TRUE))
safe_sum  <- function(x) ifelse(all(is.na(x)), 0, sum(x, na.rm = TRUE))

latest_year <- max(claims$year, na.rm = TRUE)
latest_quarter <- claims %>%
  filter(year == latest_year) %>%
  summarise(q = max(quarter, na.rm = TRUE)) %>%
  pull(q)

insurers_list <- sort(unique(claims$name_of_insurer))

ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty", # A more colorful theme
    primary = "#0066CC",
    base_font = font_google("Poppins")
  ),
  title = tagList(
    "Quarterly Insurance Claims Settlement -  Long Term Insurance Business"
  ),
  underline = FALSE,
  
  # ---- Value Boxes ----
  layout_column_wrap(
    width = "250px",
    value_box(title = "Industry Avg Ratio", value = textOutput("industry_ratio"),
              showcase = bs_icon("graph-up"), theme_color = "primary"),
    value_box(title = "Declined Claims", value = textOutput("declined_ratio"),
              showcase = bs_icon("exclamation-triangle"), theme_color = "warning"),
    value_box(title = "Number of Claims Paid", value = textOutput("claims_paid"),
              showcase = bs_icon("check-circle"), theme_color = "success"),
    value_box(title = "Outstanding Claims", value = textOutput("claims_outstanding"),
              showcase = bs_icon("hourglass-split"), theme_color = "info")
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    # ---- Left Column: Insights ----
    navset_card_tab(
      title = "📊 Insights",
      
      nav_panel(
        "Claims Mix Breakdown",
        pickerInput("mix_insurer", "Select Insurer:", choices = insurers_list,
                    selected = "Industry", width = "300px"),
        card(full_screen = TRUE,
             card_body(plotlyOutput("mix_plot")))
      ),
      
      # 🔹 Trend Over Time (Multiple Insurer Selection)
      nav_panel(
        "Trend Over Time",
        pickerInput(
          "trend_insurer", 
          "Select Insurer(s):", 
          choices = insurers_list, 
          selected = c("Industry"), 
          multiple = TRUE, 
          options = list(`actions-box` = TRUE),
          width = "400px"
        ),
        card(full_screen = TRUE,
             card_body(plotlyOutput("trend_plot")))
      ),
      
      nav_panel(
        "Heatmap Comparison",
        pickerInput("heatmap_year", "Year for Heatmap:", choices = sort(unique(claims$year)),
                    selected = latest_year, width = "250px"),
        card(full_screen = TRUE,
             card_body(plotlyOutput("heatmap_plot")))
      )
    ),
    
    # ---- Right Column: Insurer Analysis ----
    navset_card_tab(
      title = "📈 Insurer Analysis",
      
      wellPanel(
        fluidRow(
          column(
            6,
            pickerInput(
              "year",
              "Year:",
              choices = sort(unique(claims$year)),
              selected = latest_year
            )
          ),
          column(
            6,
            pickerInput(
              "quarter",
              "Quarter:",
              choices = c("Q1", "Q2", "Q3", "Q4"),
              selected = latest_quarter
            )
          )
        )
      ),
      
      nav_panel("Per Insurer", card(full_screen = TRUE, card_body(plotlyOutput("bar_plot")))),
      nav_panel("Distribution", card(full_screen = TRUE, card_body(plotlyOutput("box_plot")))),
      nav_panel("Benchmarking", card(full_screen = TRUE, card_body(plotlyOutput("benchmark_plot")))),
      nav_panel("Top & Bottom 5", card(full_screen = TRUE, card_body(plotlyOutput("top_bottom_plot"))))
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    claims %>% filter(year == input$year, quarter == input$quarter)
  })
  
  # ---- Value Boxes ----
  output$industry_ratio <- renderText({
    paste0(round(safe_mean(filtered_data()$claim_settlement_ratio_percent), 1), "%")
  })
  
  output$declined_ratio <- renderText({
    paste0(round(safe_mean(filtered_data()$declined_claims_ratio_percent), 1), "%")
  })
  
  output$claims_paid <- renderText({
    format(safe_sum(filtered_data()$claims_paid_during_the_quarter), big.mark = ",")
  })
  
  output$claims_outstanding <- renderText({
    format(safe_sum(filtered_data()$claims_outstanding_at_the_end_of_the_quarter), big.mark = ",")
  })
  
  # ---- Claims Mix Breakdown ----
  output$mix_plot <- renderPlotly({
    df <- claims %>%
      filter(name_of_insurer == input$mix_insurer) %>%
      group_by(year, quarter) %>%
      summarise(
        Paid = safe_sum(claims_paid_during_the_quarter),
        Declined = safe_sum(claims_declined_during_the_quarter),
        Outstanding = safe_sum(claims_outstanding_at_the_end_of_the_quarter),
        .groups = "drop"
      ) %>%
      pivot_longer(-c(year, quarter), names_to = "Type", values_to = "Value") %>%
      mutate(period = paste0(quarter, "-", year))
    
    plot_ly(df, x = ~fct_inorder(period), y = ~Value, color = ~Type,
            type = "scatter", mode = "lines+markers", line = list(width = 3)) %>%
      layout(title = paste("Claims Mix Breakdown -", input$mix_insurer),
             xaxis = list(title = "Quarter-Year"),
             yaxis = list(title = "Number of Claims"),
             legend = list(title = list(text = "Claim Type")))
  })
  
  # ---- Trend Over Time ----
  output$trend_plot <- renderPlotly({
    q_map <- c(Q1 = 1, Q2 = 2, Q3 = 3, Q4 = 4)
    
    df <- claims %>%
      filter(name_of_insurer %in% input$trend_insurer) %>%
      mutate(q_num = q_map[quarter], period = paste0(quarter, "-", year)) %>%
      arrange(year, q_num)
    
    plot_ly(df, x = ~fct_inorder(period), y = ~claim_settlement_ratio_percent,
            color = ~name_of_insurer, type = "scatter", mode = "lines+markers",
            line = list(width = 3)) %>%
      layout(title = "Claim Settlement Ratio Trend Over Time",
             xaxis = list(title = "Quarter-Year"),
             yaxis = list(title = "Settlement Ratio (%)"),
             legend = list(title = list(text = "Insurer")))
  })
  
  # ---- Heatmap ----
  output$heatmap_plot <- renderPlotly({
    df <- claims %>% filter(year == input$heatmap_year, name_of_insurer != "Industry")
    
    plot_ly(df, x = ~quarter, y = ~name_of_insurer,
            z = ~claim_settlement_ratio_percent, type = "heatmap",
            colors = "YlGnBu", colorbar = list(title = "Settlement Ratio (%)")) %>%
      layout(title = paste("Insurer Settlement Ratios -", input$heatmap_year),
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Insurer"))
  })
  
  # ---- Per Insurer ----
  output$bar_plot <- renderPlotly({
    df <- filtered_data() %>% filter(name_of_insurer != "Industry")
    
    plot_ly(df, x = ~reorder(name_of_insurer, claim_settlement_ratio_percent),
            y = ~claim_settlement_ratio_percent, type = "bar",
            marker = list(color = "steelblue")) %>%
      layout(title = paste("Settlement Ratio per Insurer -", input$quarter, input$year),
             xaxis = list(title = "Insurer", tickangle = -45),
             yaxis = list(title = "Settlement Ratio (%)"))
  })
  
  # ---- Distribution ----
  output$box_plot <- renderPlotly({
    df <- filtered_data() %>% filter(name_of_insurer != "Industry")
    
    plot_ly(df, x = ~reorder(name_of_insurer, claim_settlement_ratio_percent),
            y = ~claim_settlement_ratio_percent, type = "box",
            boxpoints = "all", jitter = 0.3,
            marker = list(color = "darkorange")) %>%
      layout(title = paste("Distribution of Settlement Ratios -", input$quarter, input$year),
             xaxis = list(title = "Insurer", tickangle = -45),
             yaxis = list(title = "Settlement Ratio (%)"))
  })
  
  # ---- Benchmarking ----
  output$benchmark_plot <- renderPlotly({
    df <- filtered_data() %>% filter(name_of_insurer != "Industry")
    industry_avg <- safe_mean(df$claim_settlement_ratio_percent)
    df <- df %>% mutate(RelativePerf = claim_settlement_ratio_percent - industry_avg)
    
    plot_ly(df, x = ~reorder(name_of_insurer, RelativePerf), y = ~RelativePerf,
            type = "bar", marker = list(color = ifelse(df$RelativePerf > 0, "green", "red"))) %>%
      layout(title = paste("Performance vs Industry Avg -", input$quarter, input$year),
             xaxis = list(title = "Insurer", tickangle = -45),
             yaxis = list(title = "Performance Difference (%)"))
  })
  
  # ---- Top & Bottom 5 ----
  output$top_bottom_plot <- renderPlotly({
    df <- filtered_data() %>% filter(name_of_insurer != "Industry")
    top5 <- df %>% arrange(desc(claim_settlement_ratio_percent)) %>% head(5) %>% mutate(Category = "Top 5")
    bottom5 <- df %>% arrange(claim_settlement_ratio_percent) %>% head(5) %>% mutate(Category = "Bottom 5")
    combined <- bind_rows(top5, bottom5)
    
    plot_ly(combined, x = ~reorder(name_of_insurer, claim_settlement_ratio_percent),
            y = ~claim_settlement_ratio_percent, color = ~Category, type = "bar") %>%
      layout(title = paste("Top & Bottom 5 Insurers -", input$quarter, input$year),
             xaxis = list(title = "Insurer", tickangle = -45),
             yaxis = list(title = "Settlement Ratio (%)"))
  })
}

shinyApp(ui, server)
