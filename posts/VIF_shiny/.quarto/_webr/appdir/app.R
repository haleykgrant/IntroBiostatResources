library(shiny)
library(tidyverse)
#library(GGally)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Added-Variable Plots and VIF"),
  
  
  # Show a plot of the generated distribution
  tabsetPanel(type = "tabs",
              tabPanel("Changing the variability in x",
                       sidebarLayout( 
                         sidebarPanel(
                           sliderInput("c", "Scaling Constant",  min = 0.1, max = 3,value = 1)),
                         mainPanel( 
                           plotOutput("xplot",height = "300px") )))
  ) 
  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  set.seed(234)
  N = 150
  x1 = runif(n = N, max = 10) %>% round()
  y = 4.2 + 1.5*x1 + rnorm(n = N, sd = 1.3)
 

 # vals<-reactiveValues( x2=x2,c=1,av.data=NULL,dt=data.frame(x1, x2 =x2,y),cor = 0.75)
  
  vals<-reactiveValues(c=1)
  
  observeEvent({input$c},{ vals$c=input$c})
  
 
  output$xplot <- renderPlot({
    dt = data.frame(x1,y) %>% mutate(x1c = x1*vals$c)
    dt %>% 
      ggplot(aes(x = x1c, y=y)) + 
      geom_point() +
      theme_bw() + 
      labs(x = paste0(vals$c," * x")) + 
      xlim(0, 30) + 
      geom_smooth(method = "lm", formula = y~x, se = F)
  })
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
