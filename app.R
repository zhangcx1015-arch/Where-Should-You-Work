library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(scales)
library(readr)

state_data <- read_csv("state_data_clean.csv", show_col_types = FALSE)

state_data <- state_data %>%
  mutate(
    real_salary = average_salary / (price_index / 100),
    rent_share = median_rent * 12 / average_salary * 100
  )

ui <- fluidPage(
  
  titlePanel("Where Should You Work?"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput(
        "states",
        "Choose states to compare (up to 5):",
        choices = state_data$state,
        selected = c("California", "Texas", "Nevada", "New York", "Florida"),
        multiple = TRUE,
        options = list(maxItems = 5)
      ),
      
      sliderInput(
        "salary_input",
        "Expected salary:",
        min = 40000,
        max = 120000,
        value = 70000,
        step = 5000,
        pre = "$",
        sep = ","
      ),
      
      radioButtons(
        "metric",
        "Choose comparison:",
        choices = c(
          "Real Salary",
          "Rent Burden",
          "Unemployment Rate"
        )
      ),
      
      helpText(
        "Data sources: BLS OEWS wage data, BLS LAUS unemployment data, Census ACS median gross rent, and BEA Regional Price Parities. Price index uses U.S. average = 100."
      )
    ),
    
    mainPanel(
      plotOutput("main_plot"),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$states)
    
    state_data %>%
      filter(state %in% input$states) %>%
      mutate(
        user_expected_salary = input$salary_input,
        user_real_salary = input$salary_input / (price_index / 100),
        user_rent_share = median_rent * 12 / input$salary_input * 100
      )
  })
  
  output$main_plot <- renderPlot({
    
    plot_data <- filtered_data()
    
    if (input$metric == "Real Salary") {
      
      ggplot(plot_data, aes(x = reorder(state, user_real_salary), y = user_real_salary)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        scale_y_continuous(labels = dollar_format()) +
        theme_minimal() +
        labs(
          title = "Real Purchasing Power by State",
          subtitle = paste("Based on expected salary:", dollar(input$salary_input)),
          x = "State",
          y = "Cost-of-living adjusted salary"
        )
      
    } else if (input$metric == "Rent Burden") {
      
      ggplot(plot_data, aes(x = reorder(state, user_rent_share), y = user_rent_share)) +
        geom_col(fill = "darkred") +
        coord_flip() +
        scale_y_continuous(labels = percent_format(scale = 1)) +
        theme_minimal() +
        labs(
          title = "Share of Salary Spent on Rent",
          subtitle = paste("Based on expected salary:", dollar(input$salary_input)),
          x = "State",
          y = "Annual rent as % of expected salary"
        )
      
    } else {
      
      ggplot(plot_data, aes(x = reorder(state, unemployment_rate), y = unemployment_rate)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        scale_y_continuous(labels = percent_format(scale = 1)) +
        theme_minimal() +
        labs(
          title = "Unemployment Rate by State",
          subtitle = "Used as a state-level labor market context indicator",
          x = "State",
          y = "Unemployment rate"
        )
    }
  })
  
  output$table <- renderDT({
    
    table_data <- filtered_data() %>%
      transmute(
        State = state,
        `Average Salary` = round(average_salary),
        `Monthly Median Rent` = round(median_rent),
        `Unemployment Rate (%)` = round(unemployment_rate, 1),
        `Price Index (US = 100)` = round(price_index, 1),
        `State Average Real Salary` = round(real_salary),
        `Rent Share Based on State Avg Salary (%)` = round(rent_share, 1),
        `User Expected Salary` = round(user_expected_salary),
        `User Real Salary` = round(user_real_salary),
        `User Rent Share (%)` = round(user_rent_share, 1)
      )
    
    datatable(
      table_data,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(
        columns = c(
          "Average Salary",
          "Monthly Median Rent",
          "State Average Real Salary",
          "User Expected Salary",
          "User Real Salary"
        ),
        currency = "$",
        digits = 0
      )
  })
}

shinyApp(ui = ui, server = server)