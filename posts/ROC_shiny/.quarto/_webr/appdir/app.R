library(shiny)
library(dplyr)
library(ggplot2)
library(pROC)

# Simulated data
generate_simulated_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    ID = 1:n,
    TrueLabel = sample(c(0, 1), n, replace = TRUE),
    Score = runif(n)
  )
}

ui <- fluidPage(
  titlePanel("Classification with Custom Data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Data Source:",
                   choices = c("Simulated", "User-Provided")),
      
      conditionalPanel(
        condition = "input.data_source == 'User-Provided'",
        radioButtons("user_input_method", "Input Method:",
                     choices = c("Upload Full CSV", 
                                 "Manual Entry", 
                                 "Upload Outcomes and Scores Separately"))
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'User-Provided' && input.user_input_method == 'Upload Full CSV'",
        fileInput("csv_file", "Upload CSV", accept = ".csv")
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'User-Provided' && input.user_input_method == 'Manual Entry'",
        textAreaInput("manual_input", "Paste data (TrueLabel, Score):",
                      value = "0,0.2\n1,0.8\n0,0.3", rows = 5),
        actionButton("submit_btn", "Submit Data")
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'User-Provided' && input.user_input_method == 'Upload Outcomes and Scores Separately'",
        textAreaInput("outcome_text", "Enter True Labels (comma-separated):",
                      value = "0,1,1,0,1", rows = 2),
        textAreaInput("score_text", "Enter Scores (comma-separated):",
                      value = "0.2,0.9,0.8,0.1,0.7", rows = 2),
        actionButton("submit_btn", "Submit Data")
      ),
      
      radioButtons("roc_direction", "ROC Direction:",
                   choices = c("controls < cases" = "<", "controls > cases" = ">")),
      
      sliderInput("threshold", "Classification Threshold", min = 0, max = 1, value = 0.5, step = 0.01),
      verbatimTextOutput("metrics")
    ),
    
    mainPanel(
      plotOutput("roc_plot", height = "300px"),
      plotOutput("prob_plot", height = "300px"),
      plotOutput("split_plot", height = "300px")
    )
  )
)

server <- function(input, output, session) {
  
  user_data <- reactive({
    if (input$data_source == "Simulated") {
      return(generate_simulated_data())
    } else if (input$data_source == "User-Provided" && input$user_input_method == "Upload Full CSV" && !is.null(input$csv_file)) {
      return(read.csv(input$csv_file$datapath))
    } else {
      return(NULL)
    }
  })
  
  manual_data <- eventReactive(input$submit_btn, {
    if (is.null(input$data_source) || input$data_source != "User-Provided") return(NULL)
    if (is.null(input$user_input_method)) return(NULL)
    
    tryCatch({
      if (input$user_input_method == "Manual Entry") {
        con <- textConnection(input$manual_input)
        df <- read.csv(con, header = FALSE)
        close(con)
        colnames(df) <- c("TrueLabel", "Score")
        df$ID <- 1:nrow(df)
        return(df)
        
      } else if (input$user_input_method == "Upload Outcomes and Scores Separately") {
        y <- as.numeric(unlist(strsplit(input$outcome_text, ",")))
        x <- as.numeric(unlist(strsplit(input$score_text, ",")))
        
        if (length(y) != length(x)) stop("Outcome and score vectors must have the same length.")
        if (any(is.na(y)) || any(is.na(x))) stop("Non-numeric or missing values detected.")
        
        df <- data.frame(ID = 1:length(x), TrueLabel = y, Score = x)
        return(df)
      }
      
      return(NULL)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  final_data <- reactive({
    if (input$data_source == "User-Provided" &&
        input$user_input_method %in% c("Manual Entry", "Upload Outcomes and Scores Separately")) {
      return(manual_data())
    } else {
      return(user_data())
    }
  })
  
  processed_data <- reactive({
    df <- final_data()
    req(nrow(df) > 0)
    df %>%
      mutate(
        Prediction = ifelse(Score >= input$threshold, 1, 0),
        Type = case_when(
          Prediction == 1 & TrueLabel == 1 ~ "TP",
          Prediction == 0 & TrueLabel == 0 ~ "TN",
          Prediction == 1 & TrueLabel == 0 ~ "FP",
          Prediction == 0 & TrueLabel == 1 ~ "FN"
        )
      ) %>%
      arrange(desc(Score)) %>%
      mutate(label = factor(TrueLabel, levels = 0:1, labels = c("Control", "Case")))
  })
  
  output$prob_plot <- renderPlot({
    df <- processed_data()
    if(input$roc_direction == "<"){clrs = list(H = "blue",L = "red")
    } else {clrs = list(L = "blue",H = "red")}
    ggplot(df, aes(x = reorder(factor(ID), Score), y = Score,  color = Type)) +
      geom_point() +
      geom_hline(yintercept = input$threshold, color = "black", linetype = "dashed", size = 1) +
      scale_color_manual(values = c("TP" = "blue", "TN" = "red", "FP" = "#f08080", "FN" = "#add8e6")) +
      labs(x = "Subject", y = "Predicted Probability", fill = "Type") +
      theme_classic() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = input$threshold, ymax = 1, 
               fill = clrs$H, alpha = 0.1) + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymax = input$threshold, ymin = 0, 
               fill = clrs$L, alpha = 0.1) 
  })
  
  output$split_plot <- renderPlot({
    df <- processed_data()
    if(input$roc_direction == "<"){clrs = list(H = "blue",L = "red")
    } else {clrs = list(L = "blue",H = "red")}
    ggplot(df, aes(x = reorder(factor(ID), Score), y = Score,  color = Type)) +
      geom_point() +
      geom_hline(yintercept = input$threshold, color = "black", linetype = "dashed", size = 1) +
      scale_color_manual(values = c("TP" = "blue", "TN" = "red", "FP" = "#f08080", "FN" = "#add8e6")) +
      labs(x = "Subject", y = "Predicted Probability", fill = "Type") +
      theme_classic() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      facet_wrap(~label, scales = "free_x") + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = input$threshold, ymax = 1, 
               fill = clrs$H, alpha = 0.1) + 
      annotate("rect", xmin = -Inf, xmax = Inf, ymax = input$threshold, ymin = 0, 
               fill = clrs$L, alpha = 0.11) 
  })
  
  output$roc_plot <- renderPlot({
    df <- final_data()
    req(nrow(df) > 0)
    roc_obj <- roc(df$TrueLabel, df$Score, direction = input$roc_direction)
    ggroc(roc_obj) +
      geom_vline(xintercept = input$threshold, linetype = "dashed", color = "black") +
      labs(title = "ROC Curve", x = "Specificity", y = "Sensitivity") +
      theme_minimal() +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed")
    
  })
  
  output$metrics <- renderPrint({
    df <- final_data()
    req(nrow(df) > 0)
    roc_obj <- roc(df$TrueLabel, df$Score, direction = input$roc_direction)
    ts <- roc_obj$thresholds[is.finite(roc_obj$thresholds)]
    thresh <- which.min(abs(ts - as.numeric(input$threshold)))
    coords_res <- coords(roc_obj, x = ts[thresh], input = "threshold", ret = c("sensitivity", "specificity"))
    auc_val <- auc(roc_obj)
    cat(sprintf("AUC: %.3f\n", auc_val))
    cat(sprintf("Sensitivity at Threshold: %.3f\n", coords_res['sensitivity']))
    cat(sprintf("Specificity at Threshold: %.3f\n", coords_res['specificity']))
  })
}

shinyApp(ui, server)
