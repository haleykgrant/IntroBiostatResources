library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(viridis)
library(tidyverse)
library(shinyWidgets)






ui <- fluidPage(
  titlePanel("Cross-Section Viewer"),
  
  # Tab layout
  tabsetPanel(
    
    # Original Plot Tab
    tabPanel("3D Data Plot",
             sidebarLayout(
               sidebarPanel(
                 # Toggle for showing the red surface (cross-section)
                 switchInput("show_cross_section", label = "Show Cross-Section", value = TRUE),  # This is the toggle switch
                 conditionalPanel(
                   condition = "input.show_cross_section == true",  # Only show if cross-section is toggled
                   selectInput("var", "Choose variable for cross-section:",
                               choices = c("Education" = "educ", "Infant Mortality" = "inf")),
                   uiOutput("dynamic_val")  # Placeholder for dynamic slider
                 )
               ),
               mainPanel(
               #  verbatimTextOutput("colnames"),
                 plotlyOutput("plot3d", height = "700px")
               )
             )
    ),
    
    # Residuals Plot Tab
    tabPanel("Residuals Plot",
             sidebarLayout(
               sidebarPanel(
                 # 5 Regression control buttons
                 actionButton("reg_educ_outcome", "Regress out Education from Y"),
                 actionButton("reg_inf_outcome", "Regress out Infant Mortality from Y"),
                 actionButton("reg_educ_inf", "Regress out Education from X"),
                 actionButton("reg_inf_educ", "Regress out Infant Mortality from X"),
                 actionButton("reset", "Reset")
               ),
               mainPanel(
                 plotlyOutput("residuals_plot", height = "700px")
               )
             )
    )
  )
)

server <- function(input, output, session) {
   data_url <- "https://haleykgrant.github.io/tutorial_data/data/unhdd2020.rmph.rData"
 
     download.file(data_url, "unhdd2020.rmph.rData")
     load("unhdd2020.rmph.rData", verbose = T)
  
   

unhdd = unhdd %>%  
  drop_na(life, educ_mean, mort_infant) %>%  
  mutate(row_num = row_number())

m2 <- lm(life ~ educ_mean + mort_infant, data = unhdd)



# Save coefficients
intercept <- m2$coefficients["(Intercept)"]
educ_coef <- m2$coefficients["educ_mean"]
inf_coef <- m2$coefficients["mort_infant"]

# Range of x1 and x2 values to plot
educ_range <- seq(0, max(unhdd$educ_mean) + 1, length.out = 100)
inf_range <- seq(0, max(unhdd$mort_infant, na.rm = TRUE) + 1, length.out = 100)

yhat <- t(outer(educ_range, inf_range, function(x, y) intercept + educ_coef * x + inf_coef * y))
  # Reactive value to store the camera angle
  camera_eye <- reactiveVal(list(x = 1.5, y = -1.5, z = 1.5))  # Default camera angle
  resid_camera_eye <- reactiveVal(list(x = 1.5, y = -1.5, z = 1.5))
  
  # Update camera when user moves the plot
  observe({
    req(input$plot3d_camera)
    isolate({
      camera_eye(input$plot3d_camera$eye)
    })
  })
  observe({
    req(input$residuals_plot_camera)
    isolate({
      resid_camera_eye(input$residuals_plot_camera$eye)
    })
  })
  
  
  # Reset camera when cross-section variable changes
  observeEvent(input$var, {
    camera_eye(list(x = 1.5, y = -1.5, z = 1.5))  # Reset camera only when cross-section variable changes
  })
  
  observeEvent(input$reset, {
    resid_camera_eye(list(x = 1.5, y = -1.5, z = 1.5))  # Reset camera only when cross-section variable changes
  })
  
  # Render the dynamic slider based on selected variable
  output$dynamic_val <- renderUI({
    if (!input$show_cross_section) {
      return(NULL)  # Return nothing if cross-section is hidden
    }
    
    # Select the variable for the slider based on user input
    if (input$var == "educ") {
      v <- unhdd$educ_mean
      label <- "Education"
    } else if (input$var == "inf") {
      v <- unhdd$mort_infant
      label <- "Infant Mortality"
    }
    
    # Set min, max, and median for the slider
    min_val <- floor(min(v, na.rm = TRUE))
    max_val <- ceiling(max(v, na.rm = TRUE))
    default_val <- round(median(v, na.rm = TRUE))
    
    sliderInput("val", paste("Select", label, "value:"),
                min = min_val, max = max_val,
                value = default_val, step = 1)
  })
  
  # Render the original 3D plot
  output$plot3d <- renderPlotly({
    # Calculate the model surface based on the linear model (yhat)
    z_matrix <- matrix(yhat, nrow = length(educ_range), ncol = length(inf_range))
    
    # Start the plot
    p <- plot_ly() %>%
      add_surface(
        x = educ_range, y = inf_range,
        z = z_matrix,
        colorscale = list(c(0, 1), c("blue", "blue")),
        opacity = 0.3, showscale = FALSE,
        name = "Fitted Surface"
      )
    
    # If show_cross_section is TRUE, add the red surface and line
    if (input$show_cross_section) {
      if (input$var == "inf") {
        # Fix y (infant mortality), vary x (educ) and z (life)
        z_vals <- seq(min(yhat), max(yhat), length.out = 50)
        x_matrix <- matrix(rep(educ_range, each = length(z_vals)), nrow = length(z_vals))
        y_matrix <- matrix(input$val, nrow = length(z_vals), ncol = length(educ_range))
        z_matrix_red <- matrix(rep(z_vals, times = length(educ_range)), nrow = length(z_vals))
        
        # Model prediction along educ_range
        z_line <- intercept + educ_coef * educ_range + inf_coef * input$val
        
        x_line <- educ_range
        y_line <- rep(input$val, length(educ_range))
        z_line <- z_line
        
        p <- p %>%
          add_surface(
            x = x_matrix, y = y_matrix, z = z_matrix_red,
            colorscale = list(c(0, 1), c("grey", "grey")),
            opacity = 0.25, showscale = FALSE,
            name = "Cross-section"
          ) %>%
          add_trace(
            x = x_line,
            y = y_line,
            z = z_line,
            type = "scatter3d",
            mode = "lines",
            line = list(color = "blue", width = 5),
            name = "Slope"
          )
        
      } else if (input$var == "educ") {
        # Fix x (education), vary y (infant mortality) and z (life expectancy)
        z_vals <- seq(min(yhat), max(yhat), length.out = 50)
        y_matrix <- matrix(rep(inf_range, each = length(z_vals)), nrow = length(z_vals))
        x_matrix <- matrix(input$val, nrow = length(z_vals), ncol = length(inf_range))
        z_matrix_red <- matrix(rep(z_vals, times = length(inf_range)), nrow = length(z_vals))
        
        # Model prediction along inf_range
        z_line <- intercept + educ_coef * input$val + inf_coef * inf_range
        
        x_line <- rep(input$val, length(inf_range))
        y_line <- inf_range
        z_line <- z_line
        
        p <- p %>%
          add_surface(
            x = x_matrix, y = y_matrix, z = z_matrix_red,
            colorscale = list(c(0, 1), c("grey", "grey")),
            opacity = 0.5, showscale = FALSE,
            name = "Cross-section"
          ) %>%
          add_trace(
            x = x_line,
            y = y_line,
            z = z_line,
            type = "scatter3d",
            mode = "lines",
            line = list(color = "blue", width = 5),
            name = "Slope"
          )
      }
    }
    
    # Apply the camera angle when rendering the plot
    p %>%
      add_markers(
        x = unhdd$educ_mean, y = unhdd$mort_infant, z = unhdd$life,
        marker = list(
          color = unhdd$mort_infant, showscale = FALSE,
          colorscale = "Viridis", size = unhdd$educ_mean,
          line = list(color = 'rgba(0,0,0,0)', width = 0)
        ),
        hoverinfo = "text",  # Only show custom text (not the default x, y, z)
        # Custom text with row number included
        text = paste(
          "ID:", unhdd$row_num, "<br>"  # Add row number
        ),
        name = "Observed Data"
      ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Education", showline = TRUE),
          yaxis = list(title = "Infant Mortality", showline = TRUE),
          zaxis = list(title = "Life Expectancy", showline = TRUE),
          camera = list(eye = camera_eye())  # Use the reactive camera angle
        )
      ) %>%
      htmlwidgets::onRender("
      function(el, x) {
        el.on('plotly_relayout', function(eventData) {
          if (eventData['scene.camera']) {
            Shiny.setInputValue('plot3d_camera', eventData['scene.camera'], {priority: 'event'});
          }
        });
      }
    ")
  })
  
  # Initialize with original data
  initial_df <- unhdd %>%
    mutate(y = life, x1 = educ_mean, x2 = mort_infant) 
  
  
  
  residual_state <- reactiveVal(initial_df)
  
  # Button 1: Regress Y ~ Education (update y)
  observeEvent(input$reg_educ_outcome, {
    df <- residual_state()
    df$y <- resid(lm(y ~ x1, data = df))
    residual_state(df)
  })
  
  # Button 2: Regress Y ~ Infant Mortality (update y)
  observeEvent(input$reg_inf_outcome, {
    df <- residual_state()
    df$y <- resid(lm(y ~ x2, data = df))
    residual_state(df)
  })
  
  # Button 3: Regress Infant Mortality (x2) ~ Education (x1) (update x2)
  observeEvent(input$reg_educ_inf, {
    df <- residual_state()
    df$x2 <- resid(lm(x2 ~ x1, data = df))
    residual_state(df)
  })
  
  # Button 4: Regress Education (x1) ~ Infant Mortality (x2) (update x1)
  observeEvent(input$reg_inf_educ, {
    df <- residual_state()
    df$x1 <- resid(lm(x1 ~ x2, data = df))
    residual_state(df)
  })
  
  # Reset everything
  observeEvent(input$reset, {
    residual_state(initial_df)
  })
  
  # Render the plot using the current residual state
  output$residuals_plot <- renderPlotly({
    data <- residual_state()
    
    plot_ly() %>%
      add_markers(
        x = data$x1, y = data$x2, z = data$y,
        marker = list(
          color = data$mort_infant,
          colorscale = "Viridis",
          size = data$educ_mean,
          showscale = FALSE,
          line = list(color = 'rgba(0,0,0,0)', width = 0)
        ),
        hoverinfo = "text",
        text = paste("ID:", data$row_num),
        name = "Residualized Points"
      ) %>%
      layout(
        scene = list(
          xaxis = list(title = "X1 (Education)"),
          yaxis = list(title = "X2 (Infant Mortality)"),
          zaxis = list(title = "Y (Life Expectancy)"),
          camera = list(eye = resid_camera_eye())
        )
      )%>%
      htmlwidgets::onRender("
      function(el, x) {
        el.on('plotly_relayout', function(eventData) {
          if (eventData['scene.camera']) {
            Shiny.setInputValue('residuals_plot_camera', eventData['scene.camera'], {priority: 'event'});
          }
        });
      }
    ")
  })
  
}

shinyApp(ui, server)
