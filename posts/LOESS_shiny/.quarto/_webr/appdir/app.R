#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("LOESS Smoother"),
  
             sidebarLayout(
               sidebarPanel(
                 sliderInput("x0",
                             HTML(paste0("x",tags$sub("0"))),
                             min = 10,
                             max = 25,
                             value = 15, step = .01),
                 sliderInput("alpha",
                             label = HTML("&alpha;:"),
                             min = 0.05,
                             max = 3,
                             value = 0.75, step = .01),
                 selectInput("deg",
                             label = "degree",
                             choices = c(1,2), selected = 2)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("loessplot"),
                 plotOutput("loessplot2")
                
               )
  )
  )




# Define server logic required to draw a histogram
server <- function(input, output) {

 data_url <- "https://haleykgrant.github.io/tutorial_data/data/bmd.csv"
 

     download.file(data_url, "bmd.csv")
     df = read.csv("bmd.csv")
  


 
  
  
  
  x = df$age[df$sex=="Female"]
  y = df$relative_diff[df$sex=="Female"]
  trim <- ceiling(0.1 * length(x))
  N = length(x)
  sv <-
    sqrt(apply(apply(x%>%as.matrix(), 2L, sort)[seq(trim+1, N-trim), , drop = FALSE],
               2L, var))
  
  df0 = data.frame(x=x, y=y, x0 = 15)%>% 
    mutate(
           dist = abs(x-x0)/(sv),
           dist.rank = rank(dist),
           nbr = (dist.rank <= N*.75)%>%as.numeric(),
           maxdist = max(dist*nbr),
           wt = nbr*(1-(dist/maxdist)^3)^3)
  
  fit = lm(y~poly(x,degree = 2), weights = df0$wt)

  

  pred0 = data.frame(x = df0$x,
                         pred = predict(fit, 
                                        newdata = data.frame(x = df0$x))) %>%
    mutate(
      dist = abs(x-15)/(sv),
      dist.rank = rank(dist),
      nbr = (dist.rank <= N*.75)%>%as.numeric(),
      maxdist = max(dist*nbr),
      wt = nbr*(1-(dist/maxdist)^3)^3)
  
  predx0 = data.frame(x = 15, y = predict(fit, data.frame(x=15)))
  vals<-reactiveValues(df = df0, pred = pred0, predx0 = predx0)
  observeEvent({ input$x0
    input$alpha
    input$deg},
    { 
    
    
    if(input$alpha<1){
    vals$df = data.frame(x=x, y=y, x0 = input$x0)%>% 
      mutate(s = sd(x),
             dist = abs(x-x0)/(sv),
             dist.rank = rank(dist),
             nbr = (dist.rank <= N*input$alpha)%>%as.numeric(),
             maxdist = max(dist*nbr),
             wt = nbr*(1-(dist/maxdist)^3)^3)
    } else{
      vals$df = data.frame(x=x, y=y, x0 = input$x0)%>% 
        mutate(s = sd(x),
               dist = abs(x-x0)/(sv),
               dist.rank = rank(dist),
               nbr = (dist.rank <= N*input$alpha)%>%as.numeric(),
               maxdist = max(dist*nbr)*input$alpha,
               wt = nbr*(1-(dist/maxdist)^3)^3) }
    
    fit = lm(y~poly(x,degree = input$deg%>%as.numeric()), weights = vals$df$wt)
    
    #sv = sd(vals$df$x)
    vals$pred = data.frame(x = df0$x,
                           pred = predict(fit, 
                                          newdata = data.frame(x = df0$x))) %>%
     mutate( dist = abs(x-input$x0)/(sv),
        dist.rank = rank(dist),
       nbr = (dist.rank <= N*input$alpha)%>%as.numeric(),
       a = max(1, input$alpha),
       maxdist = max(dist*nbr)*a,
       wt = nbr*(1-(dist/maxdist)^3)^3)
    # seq(9.9, 25.6, by = 0.02)
    vals$predx0 = data.frame(x = input$x0, y = predict(fit, data.frame(x=input$x0)))
    
    
    })
  
  
  output$loessplot <- renderPlot({
    vals$df %>%
      ggplot(aes(x = x, y=y)) + 
      geom_point(aes(alpha = wt)) +
      geom_line(aes(y = pred, alpha = wt), data = vals$pred, linewidth = 2) +
      geom_vline(aes(xintercept = x0), color = "red") + 
      geom_point(aes(x=x, y = y), color = "red", shape = 4, data = vals$predx0, size = 6) +
      theme_bw() + 
      ylim(-.05,.2) +
      labs(title = "Local Polynomial Fit") + 
      theme(legend.position = "none")
     
  })
  output$loessplot2 <- renderPlot({
    #vals$df %>%
    #  ggplot(aes(x = x, y=y)) + 
    #  geom_point() +
    #  geom_smooth(method = loess, method.args = list(degree = input$deg%>%as.numeric(),
    #                                                 span = input$alpha)) +
    #  theme_bw() + 
    #  ylim(-.05,.2) +
    #  labs(title = "Full LOESS Curve")
    vals$df %>%
      ggplot(aes(x = x, y=y)) + 
      geom_point(aes(alpha = wt)) +
      geom_line(aes(y = pred, alpha = wt), data = vals$pred, linewidth = 2) +
      geom_vline(aes(xintercept = x0), color = "red") + 
      geom_point(aes(x=x, y = y), color = "red", shape = 4, data = vals$predx0, size = 6) +
      geom_smooth(method = loess, method.args = list(degree = input$deg%>%as.numeric(),
                                                     span = input$alpha)) +
      theme_bw() + 
      ylim(-.05,.2) +
      labs(title = "Local Polynomial Fit with Full LOESS Curve") + 
      theme(legend.position = "none")
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
