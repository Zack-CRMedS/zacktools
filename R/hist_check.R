hist.check <- function(x, bins = 30, n_sim = 1000, plot = TRUE, main_title = "Histogram with Normal Curve", seed = 123) {
  x <- na.omit(x)
  n <- length(x)
  mu <- mean(x)
  sigma <- sd(x)

  # Function to compute SSD statistic
  ssd_stat <- function(data, bins) {
    h <- hist(data, breaks = bins, plot = FALSE)
    hist_density <- h$density
    mids <- h$mids
    norm_density <- dnorm(mids, mean = mean(data), sd = sd(data))
    sum((hist_density - norm_density)^2) / sum(norm_density)
  }

  # Observed SSD
  observed_ssd <- ssd_stat(x, bins)

  # Bootstrap simulation of SSD under normality
  set.seed(seed)
  sim_ssds <- replicate(n_sim, {
    sim_data <- rnorm(n, mean = mu, sd = sigma)
    ssd_stat(sim_data, bins)
  })

  # Calculate p-value: proportion of simulated SSDs >= observed SSD
  p_value <- mean(sim_ssds >= observed_ssd)

  # Plot histogram + normal curve
  if (plot) {
    h <- hist(x, breaks = bins, freq = FALSE,
              main = main_title,
              xlab = "Value",
              col = zack_palette(1,1),
              border = "white")
    mids <- h$mids
    norm_density <- dnorm(mids, mean = mu, sd = sigma)
    lines(mids, norm_density, col = zack_palette(2,2), lwd = 2)
  }

  # Interpretation sentence
  alpha <- 0.05
  if (p_value > alpha) {
    result <- sprintf("The distribution approximately follows a normal distribution based on the histogram (bootstrap p-value = %.3f).", p_value)
  } else {
    result <- sprintf("The distribution does not follow a normal distribution based on the histogram (bootstrap p-value = %.3f).", p_value)
  }

  return(result)
}

