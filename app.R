library(shiny)
library(dplyr)
library(ggplot2)
library(nycflights13)

# --------------------------
# Data prep 
# --------------------------
planes_small <- planes %>%
  transmute(
    tailnum,
    manufacturer,
    engine,
    plane_year = year
  )

flights_full <- flights %>%
  left_join(weather, by = c("year", "month", "day", "hour", "origin")) %>%
  left_join(planes_small, by = "tailnum") %>%
  mutate(
    age = 2013 - plane_year,
    age_group = cut(
      age,
      breaks = c(0, 10, 20, 30, 40, 100),
      labels = c("0–10", "11–20", "21–30", "31–40", "40+")
    )
  )

top_manufacturers <- c(
  "BOEING", "AIRBUS", "CESSNA", "PIPER", "BELL",
  "ROBINSON HELICOPTER", "BOMBARDIER INC", "EMBRAER",
  "GULFSTREAM AEROSPACE", "MCDONNELL DOUGLAS"
)

# --------------------------
# Compute slopes (same logic as model)
# --------------------------
compute_slopes <- function(data, group_var, weather_var) {
  data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      slope = coef(lm(arr_delay ~ .data[[weather_var]], data = cur_data()))[2],
      p_value = summary(lm(arr_delay ~ .data[[weather_var]], data = cur_data()))$coefficients[2,4],
      n = n(),
      .groups = "drop"
    ) %>%
    rename(Group = .data[[group_var]])   # <- CLEAN GROUP NAME HERE
}

# --------------------------
# UI
# --------------------------
ui <- fluidPage(
  titlePanel("Weather Effects on Aircraft Delay — Modeling Explorer"),

  sidebarLayout(
    sidebarPanel(
      helpText("Toggle weather and aircraft groups to see slope bar plots, scatterplots, and slope table."),

      selectInput(
        "weather",
        "Weather variable:",
        choices = c(
          "Precipitation" = "precip",
          "Wind speed"    = "wind_speed",
          "Visibility"    = "visib"
        ),
        selected = "precip"
      ),

      selectInput(
        "grouping",
        "Group aircraft by:",
        choices = c(
          "Manufacturer" = "manufacturer",
          "Engine type"  = "engine",
          "Age group"    = "age_group"
        ),
        selected = "manufacturer"
      ),

      conditionalPanel(
        condition = "input.grouping == 'manufacturer'",
        checkboxGroupInput(
          "manuf_sel",
          "Select manufacturers:",
          choices = sort(top_manufacturers),
          selected = top_manufacturers
        )
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot",
                 h4("Scatterplot + Regression Lines"),
                 plotOutput("scatterPlot")
        ),

        tabPanel("Slope Bar Plot",
                 h4("Slopes by Aircraft Group"),
                 plotOutput("barPlot")
        ),

        tabPanel("Slope Table",
                 h4("Regression Slopes Table (with Group Name)"),
                 tableOutput("slopeTable")
        )
      )
    )
  )
)

# --------------------------
# SERVER
# --------------------------
server <- function(input, output, session) {

  data_filtered <- reactive({
    df <- flights_full
    if (input$grouping == "manufacturer") {
      df <- df %>% filter(manufacturer %in% input$manuf_sel)
    }
    df
  })

  # --------------------------
  # SCATTERPLOT WITH REGRESSION LINES
  # --------------------------
  output$scatterPlot <- renderPlot({
    df <- data_filtered()

    ggplot(df, aes(
      x = .data[[input$weather]],
      y = arr_delay,
      color = .data[[input$grouping]]
    )) +
      geom_point(alpha = 0.15) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      theme_minimal() +
      labs(
        x = input$weather,
        y = "Arrival Delay",
        color = input$grouping
      )
  })

  # --------------------------
  # BAR PLOT OF SLOPES
  # --------------------------
  output$barPlot <- renderPlot({
    df <- data_filtered()
    slopes <- compute_slopes(df, input$grouping, input$weather)

    ggplot(slopes, aes(
      x = Group,
      y = slope,
      fill = Group
    )) +
      geom_col() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste("Slopes for", input$weather, "by", input$grouping),
        x = input$grouping,
        y = "Slope"
      )
  })

  # --------------------------
  # SLOPE TABLE 
  # --------------------------
  output$slopeTable <- renderTable({
    compute_slopes(data_filtered(), input$grouping, input$weather)
  })
}

shinyApp(ui, server)
