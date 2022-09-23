###############################################################################
## Attempting to explain patterns of Bd load within individuals through time ##
###############################################################################

####
## Notes
####

## 1) Currently set up to explore variation in Bd growth and Immune response on Bd loads
 ## Can set uncertainty in Bd growth and/or Immune response on or off
  ## A) Turn uncertainty off for both to explore/plot single relationships
  ## B) Setting uncertainty on for just Immune response seems like a --first pass-- sensible way to explore what a population of 
   ##   bd loads may look like given variation among individuals in immune function
  ## C) Setting both on seems like a --first pass-- way of generally exploring how variable Bd loads may look given
   ##   expected uncertainty in these functional forms (which could be hard to define TBH)
   
## 2) Currently uncertainty given as a range and a Sobol sequence over the range of all parameters
 ## Not the most sensible moving forward (will want to set up some correlation matrix among parameters)


## 3) When running the code:

##### ---- A few choices need to be made at the top of "parameters" below to determine what kind of simulation to run 
 ## (for now just what processes to consider uncertainty in)

##### ---- For now need to scroll down through the "parameters" section to adjust the parameter ranges for the functional forms, later
 ## can move these parameter choices higher up in the script

####
## To Do [?]
####

## 1) Add a simple transmission model


####
## Packages
####

needed_packages <- c("dplyr", "tidyr", "ggplot2", "pomp", "gridExtra")
ll <- lapply(needed_packages, require, character.only = TRUE) 
ll %>% unlist() %>% data.frame(packages = needed_packages, loaded = .)
source("../ggplot_theme.R")


####
## Functions
####

 ## Temp -> Bd growth
bd_growth       <- function(t, B0, B1, B2, opt) { 
    ## B0 - int
    ## B1 - linear
    ## B2 - quadratic
  B0 + B1*(t - opt) + B2*(t - opt)^2
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


####
## Parameters
####


## Run one simulation (TRUE) or multiple (FALSE) to explore the effect of uncertainty
single_sim <- FALSE
if (!single_sim) {
  n_sim     <- 100
  uncer.bd  <- FALSE
  uncer.imm <- TRUE
} else {
  n_sim <- 1
  uncer.bd  <- FALSE
  uncer.imm <- FALSE
}

## Establish all parameters needed for the simulation
 ## If just exploring one option at a time, adjust here
if (single_sim) {

params <- list(
  temp = data.frame(
    n_days     = 120
  , temp_int   = 4
  , temp_slope = 0.2
  )
, bd = data.frame(
    B0  = 0.5
  , B1  = -0.01
  , B2  = -0.001
  , opt = 22
  , sim_num = 1  ## only relevant for !single_sim, but placeholder still needed
 )
, immunity = data.frame(
    A0       = -1
  , A1       = 0.005
  , A2       = 0.005
  , add_t    = 0.000
  , mult_t0  = 0.600
  , mult_t1  = 0.002
  , sim_num = 1  ## only relevant for !single_sim, but placeholder still needed
 )
, seed  = data.frame(
    day_start = 1
  , load_init = 10
 )
)

} else {
## Explore uncertainty / lots of options at once
  
## Realistically there is probably a strong correlation between the individual parameters for each functional form,
 ## however, this is a bit hard to predict, so sticking with Sobol for now
  ## Still not so sure how sensible these ranges are -- just a start here
  
params <- list(
  ## Temperature simulation. Could increase complexity later
  temp = data.frame(
    n_days     = 120
  , temp_int   = 4
  , temp_slope = 0.2
  )
  ## Initiate infection
, seed  = data.frame(
    day_start = 1
  , load_init = 10
 )
  ## Bd growth
, bd = {
  if (uncer.bd) {
  data.frame(
    B0  = c(0.2, 0.8)
  , B1  = c(-0.004, -0.02)
  , B2  = c(-0.0001, -0.002)
  , opt = c(18, 26)
 )
  } else {
   data.frame(
    B0  = 0.5
  , B1  = -0.01
  , B2  = -0.001
  , opt = 22
  , sim_num = 1  
 )
  }
}
  ## Immune response
, immunity = {
  if (uncer.imm) {
  data.frame(
    A0       = c(-0.5, -1.5)
  , A1       = c(0.001, 0.009)
  , A2       = c(0.001, 0.009)
  , add_t    = c(0.000, 0.000)
  , mult_t0  = c(0.200, 1.000)
  , mult_t1  = c(0.0005, 0.035)
 )
  } else {
  data.frame(
    A0       = -1
  , A1       = 0.005
  , A2       = 0.005
  , add_t    = 0.000
  , mult_t0  = 0.600
  , mult_t1  = 0.002
  , sim_num = 1       
 )
  }
}
)  

if (uncer.bd) {
params$bd <- sobolDesign(
  lower = params$bd[1, ] %>% unlist()
, upper = params$bd[2, ] %>% unlist()
, n_sim
) %>% mutate(
  sim_num      = seq(n())
  )
}

if (uncer.imm) {
params$immunity <- sobolDesign(
  lower = params$immunity[1, ] %>% unlist()
, upper = params$immunity[2, ] %>% unlist()
, n_sim
) %>% mutate(
  sim_num      = seq(n())
  )
}
  
}

## For now built to only consider variation in the growth rate and immune dynamics, but could
 ## easily be adjusted to cover alternative temp regimes (just follow the convention used below)
for (j in 1:n_sim) {
  
## set up what parameters will be used. Probably can clean up how this is done later
if (uncer.bd) {
  bd_parms <- j
} else {
  bd_parms <- 1
}
if (uncer.imm) {
  imm_parms <- j
} else {
  imm_parms <- 1
}

out <- data.frame(day = seq(params$temp$n_days)) %>% 
  ## Simulate Temperature -- Could imagine making this much more complicated
  mutate(temp = with(params$temp, temp_int + day*temp_slope)) %>%
  ## Determine Bd growth rate and immune response on these days and temperatures
  mutate(
    gain = with(params$bd %>% filter(sim_num == bd_parms), bd_growth(
      t = temp, B0 = B0, B1 = B1, B2 = B2, opt = opt
    ))
  , loss = with(params$immunity %>% filter(sim_num == imm_parms), immune_resp_log(
      t = temp, d = day, A0 = A0, A1 = A1, A2 = A2, add_t = add_t, mult_t0 = mult_t0, mult_t1 = mult_t1
    ))) %>%
  mutate(
    bd_change = gain - loss
  ) %>%
  ## Initiate load
  mutate(
    Bd_load = with(params$seed, ifelse(day < day_start, 0, ifelse(day > day_start, NA, load_init)))
  )

## Load dynamics
for (i in seq_along(seq(nrow(out) - 1))) {
  out$Bd_load[i+1] <- with(out[i, ], Bd_load * exp(bd_change))
}

## label so can compare to parameters
out <- out %>% mutate(sim_num = j)

## Probably not the cleanest way to do this, but w/e
if (j == 1) {
 out.f <- out
} else {
 out.f <- rbind(out.f, out)
}

## Quick print to track progress
if (((j / 20) %% 1) == 0) {
  print(paste("Through", (j/n_sim) * 100, "% of simulations"))
}

}


####
## Plots
####

## Could "waste" some more time ensuring that these line up perfeclty, but w/e
if (single_sim) {
  
gg.func <- out.f %>% dplyr::select(-Bd_load, -sim_num) %>% pivot_longer(-c(day, temp), names_to = "Component") %>%
  mutate(Component = plyr::mapvalues(Component, from = c("gain", "loss", "bd_change"), to = c("Bd Growth", "Immune Response", "Difference"))) %>% {
  ggplot(., aes(day, value)) + 
    geom_line(aes(colour = Component), size = 2) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_colour_brewer(palette = "Dark2") +
    xlab("") +
    ylab("Rate")
  }

gg.load <- out.f %>% dplyr::select(-gain, -loss, -bd_change, -sim_num) %>% pivot_longer(-c(day, temp), names_to = "Component") %>% {
    ggplot(., aes(day, value)) + 
    geom_line(aes(colour = Component), size = 2) + 
    scale_colour_manual(values = c("dodgerblue3")) +
    xlab("Day") +
    ylab("Bd Load") + 
    scale_y_log10()
}

grid.arrange(gg.func, gg.load, ncol = 1)
  
  
} else {
  
if ((uncer.bd & uncer.imm) | (uncer.bd & !uncer.imm)) {
  
gg.bd <- out.f %>% group_by(day, temp) %>% summarize(
  lwr_w_g = quantile(gain, 0.025)
, lwr_n_g = quantile(gain, 0.200)
, mid_g   = quantile(gain, 0.500)
, upr_n_g = quantile(gain, 0.800)
, upr_w_g = quantile(gain, 0.975)
) %>% {
  ggplot(., aes(temp, mid_g)) + geom_line() + 
    geom_ribbon(aes(ymin = lwr_w_g, ymax = upr_w_g), alpha = 0.3) +
    geom_ribbon(aes(ymin = lwr_n_g, ymax = upr_n_g), alpha = 0.3) +
    xlab("") + ylab("Bd growth")
}
  
gg.ir <- out.f %>% group_by(day, temp) %>% summarize(
  lwr_w_l = quantile(loss, 0.025)
, lwr_n_l = quantile(loss, 0.200)
, mid_l   = quantile(loss, 0.500)
, upr_n_l = quantile(loss, 0.800)
, upr_w_l = quantile(loss, 0.975)
) %>% {
  ggplot(., aes(temp, mid_l)) + geom_line() + 
    geom_ribbon(aes(ymin = lwr_w_l, ymax = upr_w_l), alpha = 0.3) +
    geom_ribbon(aes(ymin = lwr_n_l, ymax = upr_n_l), alpha = 0.3) +
    xlab("") + ylab("Immune 
Response")
}

gg.load <- out.f %>% group_by(day, temp) %>% mutate(Bd_load = ifelse(Bd_load < 0.1, 0, Bd_load)) %>% summarize(
  lwr_w_b = quantile(Bd_load, 0.025)
, lwr_n_b = quantile(Bd_load, 0.200)
, mid_b   = quantile(Bd_load, 0.500)
, upr_n_b = quantile(Bd_load, 0.800)
, upr_w_b = quantile(Bd_load, 0.975)
) %>% {
  ggplot(., aes(temp, mid_b)) + geom_line() + 
    geom_ribbon(aes(ymin = lwr_w_b, ymax = upr_w_b), alpha = 0.3) +
    geom_ribbon(aes(ymin = lwr_n_b, ymax = upr_n_b), alpha = 0.3) +
    scale_y_log10() +
    xlab("Temparture") + ylab("Bd load")
}

grid.arrange(gg.bd, gg.ir, gg.load, ncol = 1)

} else {
  
gg.bd <- out.f %>% filter(sim_num == 1) %>% {
  ggplot(., aes(temp, gain)) + 
    geom_line(colour = "firebrick3", size = 2) + 
    xlab("") + ylab("Bd growth")
}
  
gg.ir <- out.f %>% {
  ggplot(., aes(temp, loss)) + 
    geom_line(aes(group = sim_num), colour = "dodgerblue3", size = 0.5, alpha = 0.5) + 
    xlab("") + ylab("Immune 
Response")
}

gg.load <- out.f %>% mutate(Bd_load = ifelse(Bd_load < 0.1, 0, Bd_load)) %>% {
  ggplot(., aes(temp, Bd_load)) + 
    geom_line(aes(group = sim_num), size = 0.5, alpha = 0.5) + 
    scale_y_log10() +
    xlab("Temparture") + ylab("Bd load")
}

grid.arrange(gg.bd, gg.ir, gg.load, ncol = 1)  
  
}

}

