####
## Establish Bd Growth + Immunity vs Temperature
##   and
## Predict Bd Profiles from ^^
####

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
