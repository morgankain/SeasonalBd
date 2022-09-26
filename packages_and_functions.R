####
## Packages and Functions
####

needed_packages <- c("dplyr", "tidyr", "ggplot2", "pomp", "gridExtra")
ll <- lapply(needed_packages, require, character.only = TRUE) 
ll %>% unlist() %>% data.frame(packages = needed_packages, loaded = .) %>% print()
source("ggplot_theme.R")

 ## Temp -> Bd growth
bd_growth       <- function(t, B0, B1, B2, opt) { 
    ## B0 - int
    ## B1 - linear
    ## B2 - quadratic
  B0 + B1*(t - opt) + B2*(t - opt)^2
}
 ## Alternative Temp -> Bd growth based on some lab experiments
stinner_func <- function(c, k1, k2, opt, t) {
 ## plateau like function, steep at low and high temps, flat maxima inbetween these vals 
 (c / (1 + exp(k1 + k2 * t))) + (c / (1 + exp(k1 + k2 * (2 * opt - t)))) - c
  
## Sensible parameter values to start with (from a pub, need to refind the citation)
# bd_c   <- seq(0.5, 1.2, by = 0.1) # 0.813
# bd_k1  <- 32.364
# bd_k2  <- -2.555
# bd_opt <- 19.234
}
 ## Temp + Time -> Immune Response
immune_resp_log <- function(t, d, A0, A1, A2, add_t, mult_t0, mult_t1) {
   ## A0     - base immune stength
   ## A1     - linear temp effect
   ## A2     - quadratic temp effect
   ## add_t  - additive time effect
   ## mult_t - cumulative time effect
  add_t*d + (1/(1 + exp(-1 * ((A0 + A1*t + A2*t^2)))) * (mult_t0 + mult_t1*d))
}
 ## Transmission based on cumulative load in the population
inf_prob <- function(load, a, b, k) {
 ## simple piece-wise, between min and max, a logistic
   ## (need to give more thought to this, likely a better way to set this up)
  ## k = max (after which transmission is guaranteed)
  ## a = intercept of logistic
  ## b = slope over cumulative load
  ifelse(load == 0
    , 0
    , ifelse(load > k
    , 1
    , plogis(a + b * load))
    )
}
 ##