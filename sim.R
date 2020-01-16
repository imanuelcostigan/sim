library(tidyverse)


gbm <- function(x0, mu, sigma, step_size, n_steps) {
  crn <- cumsum(rnorm(n_steps, 0, 1))
  drift <- mu * n_steps * step_size
  diffusion <- sigma * sqrt(step_size)
  x0 * exp(drift + diffusion * crn)
}


# single path

gbm(1, 0.1, 0.5, 1/12, 5 * 12)




