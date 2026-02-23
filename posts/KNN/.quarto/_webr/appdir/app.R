library(shiny)
library(plotly)
library(FNN)
library(dplyr)
library(tidyverse)
library(caret)
library(kableExtra)

dir.create("./Data")

url.csv <- "https://haleykgrant.github.io/tutorial_data/data/trainingData_diabetes.csv"
download.file(url.csv, "./Data/trainingData_diabetes.csv.csv")

df <- read_csv("./Data/trainingData_diabetes.csv") %>%
  select(hb_a1c, bmi, class) %>% 
  arrange(class) %>%
  mutate(id = row_number(),
         class = factor(class)) 

ui <- fluidPage(
  titlePanel("KNN Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", 
                  "Number of Neighbors (K):", 
                  min = 1, max = 11, value = 3, step = 2),
      
      checkboxInput("scale_data",
                    "Scale Predictors",
                    value = FALSE)
    ),
    
    mainPanel(
      plotlyOutput("plot"),
      br(),
      uiOutput("knn_table")
    )
  )
)

server <- function(input, output, session) {
  
  selected_index <- reactiveVal(NULL)
  
  # Handle plot click
  observeEvent(event_data("plotly_click", source = "knnplot"), {
    click_data <- event_data("plotly_click", source = "knnplot")
    clicked_id <- click_data$key
    
    if (!is.null(selected_index()) && selected_index() == clicked_id) {
      selected_index(NULL)
    } else {
      selected_index(clicked_id)
    }
  })
  
  # Reactive predictors (scaled or not)
  predictors_reactive <- reactive({
    predictors <- df[, c("bmi", "hb_a1c")]
    
    if (input$scale_data) {
      predictors <- scale(predictors)
    }
    
    predictors
  })
  
  output$plot <- renderPlotly({
    req(input$k)
    
    k <- input$k
    plot_data <- df
    if(input$scale_data){plot_data[, c("bmi", "hb_a1c")] <- scale(df[, c("bmi", "hb_a1c")])}
    predictors <- predictors_reactive()
    sel <- selected_index()
    
    knn_result <- get.knn(predictors, k = k)
    
    if (!is.null(sel)) {
      row_idx <- which(plot_data$id == sel)
      neighbors <- knn_result$nn.index[row_idx, ]
      highlight_rows <- c(row_idx, neighbors)
      
      plot_data <- plot_data %>%
        mutate(
          opacity = ifelse(row_number() %in% highlight_rows, 1, 0.2),
          size = ifelse(row_number() %in% highlight_rows, 12, 6),
          symbol = ifelse(row_number() == row_idx, "x", "circle")
        )
    } else {
      plot_data <- plot_data %>%
        mutate(opacity = 1, size = 6, symbol = "circle")
    }
    
    plty <- plot_ly(
      data = plot_data,
      x = ~bmi,
      y = ~hb_a1c,
      color = ~class,
      key = ~id,
      text = ~paste0("ID: ", id,
                     "<br>BMI: ", round(bmi, 2),
                     "<br>HbA1c: ", round(hb_a1c, 2),
                     "<br>Class: ", class),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(
        size = ~size,
        opacity = ~opacity,
        symbol = ~symbol
      ),
      source = "knnplot",
      colors = c("Non.Diabetic" = "#F8766D",
                 "Pre.Diabetic" = "#00BA38",
                 "Diabetic" = "#619CFF")
    )
    
    # Apply layout separately
    if (input$scale_data) {
      plty <- plty %>%
        layout(
          xaxis = list(
            title = "BMI (Scaled)",
            scaleanchor = "y",   # lock x-axis scale to y
            scaleratio = 1       # 1:1 aspect ratio
          ),
          yaxis = list(
            title = "HbA1c (Scaled)"
          )
        )
    } else {
      plty <- plty %>%
        layout(
          xaxis = list(title = "BMI"),
          yaxis = list(title = "HbA1c")
        )
    }
  })
  
  output$knn_table <- renderUI({
    req(selected_index(), input$k)
    
    k <- input$k
    plot_data <- df
    predictors <- predictors_reactive()
    
    row_idx <- which(plot_data$id == selected_index())
    knn_result <- get.knn(predictors, k = k)
    neighbors <- knn_result$nn.index[row_idx, ]
    neighbor_classes <- plot_data$class[neighbors]
    
    tbl <- plot_data %>%
      slice(neighbors) %>%
      count(class, name = "n") %>%
      mutate(p = n / sum(n),
             assigned = names(which.max(table(neighbor_classes))))
    
    HTML(tbl %>%
           kable(digits = 3, format = "html") %>%
           kable_styling("striped", full_width = FALSE) %>%
           collapse_rows(columns = 4, valign = "middle"))
  })
}

shinyApp(ui, server)

