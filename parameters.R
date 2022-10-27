####
## Establish parameters for the within-individual piece of the model
####

## Run one simulation (TRUE) or multiple (FALSE) to explore the effect of uncertainty
single_sim <- FALSE
if (!single_sim) {
  n_sim     <- 1000
  uncer.bd  <- TRUE
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
  , A1       = c(0.001, 0.004)
  , A2       = c(0.001, 0.004)
  , add_t    = c(0.000, 0.000)
  , mult_t0  = c(0.200, 0.500)
  , mult_t1  = c(0.0005, 0.015)
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

