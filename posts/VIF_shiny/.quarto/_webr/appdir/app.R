library(shiny)
library(tidyverse)
library(GGally)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Added-Variable Plots and VIF"),
  
  
  # Show a plot of the generated distribution
  tabsetPanel(type = "tabs",
              tabPanel("Changing the variability in x",
                       strong("Description:"),
                           p("The goal of this part of the app is to show you how changing the amount of variability in x impacts the standard error of our coefficient estimate for \u03B2. That is, if we increase or decrease the amount of variability in the predictor (x) relative to the amount of variability in the outcome (y), what happens to the standard error of the slope coefficient? "),
                           p("We will demonstrate this by scaling x by a constant factor while keeping y the same."),
                           p("Below, I have simulated some data with predictor x and outcome y. Move the slider above to increase or decrease the standard deviation of x (scale by <1 to decrease and >1 to increase the standard deviation). The outcome y will stay the same\u2013only x is changing. Notice how the standard error of the slope coefficient changes as we increase or decrease the variability in x."),
                       sidebarLayout( 
                         sidebarPanel(
                           sliderInput("c", "Scaling Constant", 
                                       min = 0.1, max = 3,value = 1)),
                         mainPanel( 
                           
                           plotOutput("xplot",height = "300px"),
                           span(textOutput("sd_orig"), style="font-weight:bold"),
                           span(textOutput("se_orig"), style="font-weight:bold"),
                           span(textOutput("sd"), style="font-weight:bold; color:green"),
                           span(textOutput("se"), style="font-weight:bold; color:blue"),
                           p(),
                           strong("What's the trend?"),
                           p("As we decrease the variability in the predictor, x, we increase our uncertainty (the standard error) in our estimate of \u03B2. Likewise, the standard error decreases if we increase the variability in x.  "
                           ),
                           strong("Intuition (example: changing units)"),
                           p("As a simple example of this to provide a bit more intuition for this phenomenon, imagine that the predictor x represents height and y represents shoe size. If we measure x in inches, the variance or standard deviation of x will be smaller than if we measure x in centimeters, due to the difference in units. For example, a standard deviation of 2 inches would be equivalent to a standard deviation of about 5 centimeters (the smaller the unit, the bigger the standard deviation measured in those units). If we wanted to measure the association between height and shoe size, it makes sense that we would need a wider interval to describe our uncertainty in the expected change in shoe size for a one-inch change in height compared to the interval we need for the expected change in shoe size for a one-centimeter change in height, since a one-inch change in height is a lot bigger than a one-centimeter change! Again we see that the version of x that had the larger standard deviation (centimeters) resulted in the lower standard error for the slope estimate (and vice versa).
                             We don't just have to change the variability in X based on scale, this was just a simple example. Generally this will remain true: if you increase the amount of variability in x relative to the amount of variability in y, the standard error for the regression coefficient for x will decrease."
                           )
                         ))),
              tabPanel("MLR and VIF",
                       sidebarLayout( 
                         sidebarPanel(
                           sliderInput("cor1", "Correlation betweel x1 and x2",
                                        min = -1, max = 1, step = 0.05,
                                        value= 0.75)
                         ),
                         mainPanel(
                           h3("Simulating the data"),
                           p("Here we've simulated some data with one outcome (y) and two predictors (x1 and x2). 
                             You can control the amount of correlation between x1 and x2 using the radio buttons on the left panel"),
                           h3("Plotting the data"),
                           p("Here we can see the scatterplots between each pair of variables in the data."),
                          plotOutput("corplot",height = "300px"),
                           p("Another way to visualize the data is to plot y by x1 and show x2 using color. Here we can see that as x1 changes, so does y. 
                            Depending on the correlation we chose, we may also see a pattern between x1 and the color of the points (indicating the value fo x2)."),
                          
                              plotOutput("dataPlot1", height = "300px"),
                          
                           p("Let's imagine splitting the data based on the value of x2. 
                            Here we are looking at the relationship between x1 and y for each unique value of x2 (holding x2 constant). We can think 
                            of the slope for x1 in the MLR y ~ x1 + x2 as a sort of average of the slopes in these various plots at each value of x2."),
                           
                             plotOutput("dataPlot2", height = "300px"),
                           p("Notice how if we picked a non-zero correlation between x1 and x2, the range of values of x1 for any value we fix for x2 is 
                            smaller than the overall range of x1 that we originally saw."),
                           
                           p(),
                           h3("Added Variable Plots"),
                           p("Now imagine that we combine all of these plots together, but first we center them both in terms of x1 and y. "),
                          
                           plotOutput("dataPlot3", height = "300px"),
                           p("This is similar to what an added variable plot is (if x2 is categorical, this is exactly what an added variable plot is; if it is continuous, we add a linearity constraint to the centering process). If we combine all of the centered scatter plots above, we can see that there doesn't appear to be any relationship between x2 and either x1 or y (the colors are scattered without a clear pattern). This is what we mean by \"removing the effects of x2.\" What we are visualizing now is the relationship between x1 and y, adjusted for x2 (the relationship between x1 and y that is completely independent of x2). The overall slope for x1 in the MLR is the slope in the added variable plot, which you can think of as a sort of average of the slopes across the panels in the plot above."),
                           #fluidRow(
                           #  column(12, plotOutput("dataPlot4", height = "300px"))
                           # ),
                           
                              plotOutput("AVplot1", height = "300px"),
                           strong("How does this relate to standard error?"),
                           p("Remember that changing the variability in x impacted the standard error for our \u03B2 estimate. When we fit a multiple linear regression model we are looking at the association between each variable and the outcome, holding the other variable(s) constant. But if x1 and x2 are correlated, then for each fixed value of x2, the range of values we observe for x1 is smaller than the total amount of variability in x1 overall. So including correlated predictors in a MLR model results in higher standard errors for slope coefficients than if predictors were uncorrelated."),
                           p(" If x1 and x2 are perfectly correlated, we cannot fix one variable while moving the other. Changing one means the other must change as well. In this case, when we fix the value of x2, we know exactly what x1 will be and there is no variability left in x1 (and vice versa), so the standard error for the slope coefficients for x1 and x2 will be infinity."),
                           p(),
                           
                           h3("VIF"),
                           textOutput("vif")
                           
                           
                         ) )
              )
              
  ) 
  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  set.seed(234)
  N = 150
  x1 = runif(n = N, max = 10) %>% round()
  y = 4.2 + 1.5*x1 + rnorm(n = N, sd = 1.3)
  origx2<-(3 +rnorm(n = N, sd =.5)) 
  
  X = cbind(scale(x1),scale(origx2))
  c1 = var(X)
  chol1 = solve(chol(c1))
  newx = X %*% chol1 
  R2 = matrix(c(1,0.75, 0.75, 1), nrow = 2)
  
  chol2 = chol(R2)
  finalx = newx %*% chol2 * sd(x1) + mean(x1)
  
  x2 = finalx[,2]/1.9
  
  
  
  vals<-reactiveValues( x2=x2,c=1,av.data=NULL,dt=data.frame(x1, x2 =x2,y),cor = 0.75)
  
  observeEvent({input$c},{ vals$c=input$c})
  
  observeEvent({input$cor1},{ 
    cr = as.numeric(input$cor1)
    if(!cr%in%c(1,-1)){
      
      R2 = matrix(c(1,cr, cr, 1), nrow = 2)
      
      chol2 = chol(R2)
      finalx = newx %*% chol2 * sd(x1) + mean(x1)
      
      x2 = finalx[,2]/1.9
    } else {x2 = cr * x1}
    
    
    
    #observeEvent({input$cor1},{
    #  if(input$cor1 == "1"){x2 = (3 + 0.5*x1 ) 
    #  } else if(input$cor1 == "-1"){x2 = -(3 + 0.5*x1 ) 
    #  } else if(input$cor1 == "-0.75"){x2 = -origx2
    #  } else if(input$cor1 == "0.75"){x2 = origx2
    #  } else{x2 = (3 + rnorm(n = N, sd =1))%>% plyr::round_any(.,1) }
    
    vals$x2 = x2
    vals$dt = data.frame(x1, x2 = vals$x2,y) %>%
      mutate(r_x2 = plyr::round_any(vals$x2, .5)) 
    vals$cor = as.numeric(input$cor1)
    
    
    xres1 = round(lm(x1 ~ x2, data = vals$dt)$residuals,digits = 6)
    yres1 = round(lm(y ~ x2, data = vals$dt)$residuals , digits = 6)
    xres2 = round(lm(x2 ~ x1, data = vals$dt)$residuals, digits = 6) 
    yres2 = round(lm(y ~ x1, data = vals$dt)$residuals, digits = 6)
    
    vals$av.data = data.frame(x_axis = c(xres1, xres2), 
                              y_axis = c(yres1, yres2),
                              variable = rep(c("Removing x2", "Removing x1"), 
                                             each = 50))
  })
  
  output$xplot <- renderPlot({
    dt = data.frame(x1,y) %>% mutate(x1c = x1*vals$c)
    dt %>% 
      ggplot(aes(x = x1c, y=y)) + 
      geom_point() +
      theme_bw() + 
      labs(x = paste0(vals$c," * x")) + 
      xlim(0, 30) + 
      geom_smooth(method = "lm", formula = "y~x", se = F)
  })
  
  output$se <- renderText({
    dt = data.frame(x1,y) %>% mutate(x1c = x1*vals$c)
    se = (summary(lm(y~x1c, data = dt ))$coef[2,2]) %>% 
      round(digits=5)
    print(paste0("Standard error for slope coefficient (\u03B2): ",se))
  })
  
  output$se_orig <- renderText({
    dt = data.frame(x1,y) %>% mutate(x1c = x1*vals$c)
    se = (summary(lm(y~x1, data = dt ))$coef[2,2]) %>% 
      round(digits=5)
    print(paste0("Standard error for original slope coefficient (\u03B2): ",se))
  })
  
  
  output$sd <- renderText({
    dt = data.frame(x1,y) %>% mutate(x1c = x1*vals$c)
    sd = sd(dt$x1c) %>% round(digits = 5)
    print(paste0("Standard deviation of scaled x (",vals$c,"x): ",sd))
  })
  
  output$sd_orig <- renderText({
    sd = sd(x1) %>% round(digits = 5)
    print(paste0("Standard deviation of x (original): ",sd))
  })
  
  output$corplot <- renderPlot({
    
    #psych::pairs.panels(vals$dt, ellipses = F, hist.col = "skyblue1")
    ggpairs(vals$dt %>% select(-r_x2), 
            # upper = list(continuous = wrap("blank")),
            lower = list(continuous = "points"),
            diag = list(continuous = "densityDiag")) + 
      theme_bw()
  })
  
  output$dataPlot1 <- renderPlot({
    
    vals$dt %>% 
      ggplot(aes(x = x1, y=y, color = x2)) + 
      geom_point() +
      theme_bw() + 
      xlim(0, 10) +
      scale_color_viridis_c(option="turbo") + 
      labs(title  = "Data colored by x2 value")
  })
  
  output$dataPlot2 <- renderPlot({
    
    vals$dt %>% 
      ggplot(aes(x = x1, y=y)) + 
      geom_point(aes(color = x2)) +
      theme_bw() + 
      xlim(0, 10) +
      scale_color_viridis_c(option="turbo") + 
      facet_wrap(.~r_x2)+
      labs(title  = "Data split by x2 value") + 
      geom_smooth(method = "lm", se = F, linewidth = 0.5, aes(color = r_x2)) + 
      labs(color = "x2")
  })
  
  output$dataPlot3 <- renderPlot({
    
    vals$dt %>% 
      ggplot(aes(x = x1, y=y)) + 
      geom_point(aes(color = x2)) +
      theme_bw() + 
      xlim(0, 10) +
      scale_color_viridis_c(option="turbo") + 
      facet_wrap(.~r_x2)+
      labs(title  = "Data split by x2 value") + 
      geom_smooth(method = "lm", se = F, size = 0.5, aes(color = r_x2)) + 
      labs(color = "x2")+  
      geom_vline(data = vals$dt %>%  group_by(r_x2) %>% summarize(mean_x = mean(x1)), aes(xintercept = mean_x), linetype = "dashed", color = "black") +
      geom_hline(data = vals$dt %>% group_by(r_x2) %>% summarize(mean_y = mean(y)), aes(yintercept = mean_y), linetype = "dashed", color = "black") + 
      theme(legend.position = "none")
  })
  
  
  
  output$AVplot1 <- renderPlot({
    
    vals$av.data %>%
      filter(variable=="Removing x2") %>%
      mutate(x2 = vals$x2)%>%
      ggplot(aes(x = x_axis, y = y_axis)) + 
      theme_bw() + 
      geom_smooth(method = "lm",color = "black", se = F,linewidth = 0.5) + 
      geom_point(aes(color = x2)) + 
      # geom_smooth(method = "lm", aes(color = x2, group = x2), linewidth = 0.5, alpha = 0.1, se = F)+
      labs(x = "Residuals (x1 | x2)", y = "Residuals (Y | x2)",
           title = "Added Variable Plot",
           subtitle = "Adjusting for x2") +
      scale_color_viridis_c(option="turbo") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
      geom_vline(xintercept = 0,linetype = "dashed", color = "black")
    
  })
  
  
  
  output$vif <- renderText({
    vif = round(1/(1-as.numeric(vals$cor)^2),5)
    print(paste0("Variance Inflation Factor: ",vif))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
