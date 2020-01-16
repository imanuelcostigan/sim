library(purrr)
library(magrittr)


gbm <- function(x0, mu, sigma, step_size, n_steps, n_paths) {

  rn_paths <- n_paths %>%
    purrr::rerun(
      rnorm(n_steps)
    ) %>%
    purrr::map(
      ~ x0 * exp(n_steps * step_size * mu + sqrt(step_size) * sigma * cumsum(.))
    )
}

# single scenario

x0 <- 1
step_size <- 1 / 12
n_steps <- 5 * 12
n_paths <- 100

system.time(res_single_scenario <- gbm(x0, 0, 0.5, step_size, n_steps, n_paths))

# multiple scenarios - 100
scenarios <- list(
  mu = seq(-0.05, 0.05, length.out = 10),
  sigma = seq(0.1, 1.0, length.out = 10)
) %>%
  purrr::cross_df()

# 100 scenarios x 100 paths x 60 steps = 600,000 risk factors

system.time({
  res_multi_scenarios <- scenarios %>%
    purrr::pmap(gbm, x = 1, step_size, n_steps, n_paths)
})


#    user  system elapsed
#    0.143   0.025   0.169

# XVA scenario
step_size <- 1 / 4 # quarterly
n_steps <- 30 * 4  # thirty years
n_paths <- 10000   # vs 5000?
scenarios <- list(   # proxy for 5 curves + 5 FX rates = 10 risk factors. Scale up to nearest square
  mu = seq(-0.05, 0.05, length.out = 4),
  sigma = seq(0.1, 1.0, length.out = 4)
) %>%
  purrr::cross_df()

# Dimensionality: 16 x 10000 x (30 x 4)  = 19,200,000

system.time({
  res_multi_scenarios <- scenarios %>%
    purrr::pmap(gbm, x = 1, step_size, n_steps, n_paths)
})

#    user  system elapsed
#   2.993   0.106   3.101
#   Size: 155MB









