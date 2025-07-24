library(shiny)
library(plotly)
library(glmnet)

set.seed(123)
n <- 100
X <- matrix(rnorm(n * 2), ncol = 2)
colnames(X) <- c("x1", "x2")
beta_true <- c(3, -2)
y <- as.vector(X %*% beta_true + rnorm(n))

ui <- fluidPage(
  titlePanel("Penalized Regression"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", "Lambda (penalty strength)", min = 0.01, max = 3, value = 1, step = 0.01),
      sliderInput("alpha", "Alpha (0 = Ridge, 1 = Lasso)", min = 0, max = 1, value = 1, step = 0.05)
    ),
    mainPanel(
      plotlyOutput("msePlot", height = "700px", width = "100%")
    )
  )
)

server <- function(input, output) {
  output$msePlot <- renderPlotly({
    lambda <- input$lambda
    alpha <- input$alpha
    
    beta1_seq <- seq(beta_true[1] - 5, beta_true[1] + 5, length.out = 50)
    beta2_seq <- seq(beta_true[2] - 5, beta_true[2] + 5, length.out = 50)
    grid <- expand.grid(beta1 = beta1_seq, beta2 = beta2_seq)
    
    # Calculate MSE with NO intercept for each beta pair
    grid$MSE <- apply(grid, 1, function(b) {
      preds <- X %*% as.numeric(b)
      mean((y - preds)^2)
    })
    
    z_matrix <- matrix(grid$MSE, nrow = length(beta1_seq), byrow = FALSE)
    
    # Fit OLS with no intercept
    ols_fit <- lm(y ~ X - 1)
    beta_ols <- coef(ols_fit)
    mse_min <- min(grid$MSE)
    mse_max <- max(grid$MSE)
    
    # Fit glmnet penalized model with no intercept
    fit <- glmnet(X, y, alpha = alpha, lambda = lambda, intercept = FALSE, standardize = TRUE)
    beta_pen <- as.numeric(coef(fit))[-1]
    mse_pen <- mean((y - X %*% beta_pen)^2)
    
    # Compute penalty level 't' from the penalized solution (not scaled by n)
    L1_norm <- sum(abs(beta_pen))
    L2_norm2 <- sum(beta_pen^2)
    t <- alpha * L1_norm + (1 - alpha) * L2_norm2
    if (t == 0) t <- 1e-6
    
    plt <- plot_ly() %>%
      add_surface(
        x = ~beta1_seq,
        y = ~beta2_seq,
        z = ~z_matrix,
        colorscale = list(c(0, 1), c("lightgrey", "black")),
        showscale = FALSE,
        opacity = 0.9,
        name = "MSE Surface"
      ) %>%
      add_trace(
        x = rep(beta_ols[1], 2),
        y = rep(beta_ols[2], 2),
        z = c(mse_min, mse_max),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "red", width = 6),
        name = "Unpenalized"
      ) %>%
      add_trace(
        x = rep(beta_pen[1], 2),
        y = rep(beta_pen[2], 2),
        z = c(mse_min, mse_max),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "turquoise", width = 6),
        name = "Penalized"
      )
    
    ## === Constraint Wall === ##
    if (alpha == 1) {
      # === Lasso: Diamond (L1 norm constraint) ===
      angles <- seq(0, 2 * pi, length.out = 200)
      diamond <- data.frame(
        beta1 = t * cos(angles) / (abs(cos(angles)) + abs(sin(angles))),
        beta2 = t * sin(angles) / (abs(cos(angles)) + abs(sin(angles)))
      )
      
      n <- nrow(diamond)
      beta1_mat <- matrix(rep(diamond$beta1, length(z_vals)), nrow = n)
      beta2_mat <- matrix(rep(diamond$beta2, length(z_vals)), nrow = n)
      z_mat <- matrix(rep(z_vals, each = n), nrow = n)
      
      plt <- plt %>%
        add_surface(
          x = ~beta1_mat,
          y = ~beta2_mat,
          z = ~z_mat,
          opacity = 0.3,
          surfacecolor = matrix(1, nrow = n, ncol = length(z_vals)),
          colorscale = list(c(0, 1), c("turquoise", "turquoise")),
          showscale = FALSE,
          name = "Lasso Constraint"
        )
      
    } else if (alpha == 0) {
      # === Ridge: Circle (L2 norm constraint) ===
      angles <- seq(0, 2 * pi, length.out = 200)
      radius <- sqrt(t)
      circle <- data.frame(
        beta1 = radius * cos(angles),
        beta2 = radius * sin(angles)
      )
      
      n <- nrow(circle)
      beta1_mat <- matrix(rep(circle$beta1, length(z_vals)), nrow = n)
      beta2_mat <- matrix(rep(circle$beta2, length(z_vals)), nrow = n)
      z_mat <- matrix(rep(z_vals, each = n), nrow = n)
      
      plt <- plt %>%
        add_surface(
          x = ~beta1_mat,
          y = ~beta2_mat,
          z = ~z_mat,
          opacity = 0.3,
          surfacecolor = matrix(1, nrow = n, ncol = length(z_vals)),
          colorscale = list(c(0, 1), c("turquoise", "turquoise")),
          showscale = FALSE,
          name = "Ridge Constraint"
        )
      
    } else {
      # === Elastic Net: Numerical Solution for Boundary ===
      theta <- seq(0, 2 * pi, length.out = 200)
      
      get_radius <- function(theta, t, alpha) {
        fn <- function(r) {
          b1 <- r * cos(theta)
          b2 <- r * sin(theta)
          alpha * (abs(b1) + abs(b2)) + (1 - alpha) * (b1^2 + b2^2) - t
        }
        uniroot(fn, lower = 1e-6, upper = 1e3)$root
      }
      
      radii <- sapply(theta, get_radius, t = t, alpha = alpha)
      
      beta1_enet <- radii * cos(theta)
      beta2_enet <- radii * sin(theta)
      
      n <- length(beta1_enet)
      beta1_mat <- matrix(rep(beta1_enet, length(z_vals)), nrow = n)
      beta2_mat <- matrix(rep(beta2_enet, length(z_vals)), nrow = n)
      z_mat <- matrix(rep(z_vals, each = n), nrow = n)
      
      plt <- plt %>%
        add_surface(
          x = ~beta1_mat,
          y = ~beta2_mat,
          z = ~z_mat,
          opacity = 0.3,
          surfacecolor = matrix(1, nrow = n, ncol = length(z_vals)),
          colorscale = list(c(0, 1), c("turquoise", "turquoise")),
          showscale = FALSE,
          name = "Elastic Net Constraint"
        )
    }
    
    plt %>%
      layout(
        scene = list(
          xaxis = list(title = "Beta 1"),
          yaxis = list(title = "Beta 2"),
          zaxis = list(title = "Loss Function")
        )
      )
  })
}



shinyApp(ui, server)

