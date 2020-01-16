library(tidyverse)


gbm <- function(x0, mu, sigma, step_size, n_steps, n_paths) {

  rn_paths <- n_paths %>%
    purrr::rerun(
      rnorm(n_steps)
    ) %>%
    purrr::map(
      ~ x0 * exp(n_steps * step_size * mu + sqrt(step_size) * sigma * cumsum(.))
    )
}


# global variables

x0 <- 1
step_size <- 1 / 12
n_steps <- 5 * 12
n_paths <- 100

# single scenario

system.time(res_single_scenario <- gbm(x0, 0, 0.5, step_size, n_steps, n_paths))

# multiple scenarios
system.time({
  res_multi_scenarios <- list(
    mu = seq(-0.05, 0.05, length.out = 10),
    sigma = seq(0.1, 1.0, length.out = 10)
  ) %>%
    purrr::cross_df() %>%
    purrr::pmap(gbm, x = 1, step_size, n_steps, n_paths)
})


# XVA scenario







