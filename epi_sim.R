####
## Try a simple SIR-style transmission model with load dependent transmission
## (essentially a custom IPM)
####

####
## Take the Bd growth and Immune Response vs Temp relationships established
## from before and adjust the seeding parameters to better conform to a epi sim
## Use data from Scotia Barrens
####

###
## Temp data and bd swab data from Scotia Barrens, pre-processed from full data set
###
SB_temp <- read.csv("ScotiaBarrens.csv") %>% filter(year == 2020) %>% dplyr::select(-dayl)
SB_bd   <- read.csv("SB_bd.csv")

###
## Simulation parameters
###

## Could also add these to the larger parameter list to explore variation across these as well,
 ## but may be too much uncertainty for now...

## pop size, length of time, starting conditions
n_days  <- 365; days <- seq(n_days)
n_pop   <- 500
## all individuals start S, but some 'start_I' number of individuals are assumed to start infected 
 ## (i.e., have gained infection from elsewhere and entered the population)
start_I <- 1

if (!uncer.init) {

## Conditions of starting infected individuals
params$seed   <- data.frame(
  start_day_m   = 80
, start_day_sd  = 6
, start_load_m  = 100
, start_load_sd = 6
)

} else {
  
params$seed   <- data.frame(
  start_day_m   = c(50, 140)
, start_day_sd  = c(1, 8)
, start_load_m  = c(10, 1000)
, start_load_sd = c(1, 8)
)

params$seed <- sobolDesign(
  lower = params$seed[1, ] %>% unlist()
, upper = params$seed[2, ] %>% unlist()
, n_sim
) %>% mutate(
  sim_num      = seq(n())
  )
  
}

if (!uncer.trans) {
  
params$trans <- data.frame(
  tra_a = -7
, tra_b = 0.05
, tra_k = 1400
)  
  
} else {
  
params$trans <- data.frame(
  tra_a = c(-9, -3)
, tra_b = c(0.01, 0.20)
, tra_k = c(800, 3000)
)  

params$trans <- sobolDesign(
  lower = params$trans[1, ] %>% unlist()
, upper = params$trans[2, ] %>% unlist()
, n_sim
) %>% mutate(
  sim_num      = seq(n())
  )
  
}

## Storage container for the predictions
pred.out <- matrix(
  data = 0
, nrow = n_sim                            ## Number of simulations
, ncol = unique(SB_bd$JD) %>% length()    ## Days various Scotia Barrens ponds were visited (in 2020)
, dimnames = list(
  seq(n_sim)
, sort(sort(unique(SB_bd$JD)))
)
)

for (k in 1:n_sim) {

## Track Susceptible [S] individuals
S    <- numeric(n_days)
S[1] <- n_pop

## Track the number of Infected [I] individuals
I    <- matrix(
  data = 0
, ncol = n_days
, nrow = n_pop)

## Track the load of these infected individuals
I_load  <- matrix(
  data = 0
, ncol = n_days
, nrow = n_pop)

## Some randomness in Initial conditions
ind_date <- round(rnorm(start_I, params$seed$start_day_m[k], params$seed$start_day_sd[k]))
bd_start <- round(rnorm(start_I, params$seed$start_day_m[k], params$seed$start_day_sd[k]))

for (j in 1:start_I) {
 I[j, ind_date[j]]      <- 1
 I_load[j, ind_date[j]] <- bd_start[j]
}
 S[1:(min(ind_date) - 1)] <- S[1]
 S[min(ind_date)]   <- S[1] - 1

temp_inf.t <- numeric(n_days)

## Disease simulation, don't extend into the offseason, so somewhat arbitrarily stopping at day 250
for (d in (min(ind_date) + 1):250) {  ## d being the current day (predicting today based on d - 1)
  
## Check number infected individuals
num_inf       <- sum(I[, d - 1])
  
## Transmission Rate
temp_load     <- (sum(I_load[, d - 1] * I[, d - 1]) + 1) %>% log() %>% round()
temp_inf      <- inf_prob(temp_load, params$trans$tra_a[k], params$trans$tra_b[k], params$trans$tra_k[k])
temp_inf.t[d] <- temp_inf
  
## Susceptible Infection (each S individual assumed to get infected with the above calculated probability of infection)
new_inf   <- rbinom(n = 1, size = S[d - 1], prob = temp_inf) 
S[d]      <- S[d - 1] - new_inf

## Progression of disease in all of those infected individuals
if (num_inf > 0) {
  
#Change in Bd load in all individuals based on the temp and how long they have been infected
delta_bd   <- I_load[, d - 1] * exp(
  ## gain due to Bd growth | temp
  with(params$bd[k, ], bd_growth(
    t = rep((SB_temp %>% filter(yday == d))$tmean, n_pop)
  , B0 = B0, B1 = B1, B2 = B2, opt = opt
  )) - 
  ## loss due to immune response | temp
  with(params$immunity[k, ], immune_resp_log(
      t = rep((SB_temp %>% filter(yday == d))$tmean, n_pop)
    , d = rowSums(I)
    , A0 = A0, A1 = A1, A2 = A2, add_t = add_t, mult_t0 = mult_t0, mult_t1 = mult_t1
    ))
)
} else {
delta_bd   <- 0
}

## Update loads for disease progression 
I_load[, d] <- delta_bd

## Update matrix of infected individuals and load based on newly infected individuals
I[, d] <- I[, d - 1]
if (new_inf > 0) {
 I[(num_inf + 1):(num_inf + new_inf), d]      <- 1
 I_load[(num_inf + 1):(num_inf + new_inf), d] <- rnorm(new_inf, params$seed$start_load_m[k], params$seed$start_load_sd[k])
}
 
}

## Vague form of an objective function to maximize
obj_max <- left_join(
  
SB_bd %>% group_by(JD) %>% summarize(
  mean_load_m = mean(bd_load + 1) %>% log()
, sd_load_m   = sd(log_bd_load)
, .groups = "keep"
)
  
, (I_load[, unique(SB_bd$JD) %>% sort()] + 1) %>% 
  log() %>% reshape2::melt() %>% rename(Mark = Var1, JD = Var2) %>%
  mutate(JD = plyr::mapvalues(JD, from = unique(JD), to = unique(SB_bd$JD) %>% sort())) %>%
  group_by(JD) %>% summarize(
  mean_load_e = mean(value)
, sd_load_e   = sd(value)
, .groups = "keep"
)

, by = "JD"
  
) %>% mutate(
  dif_mean = mean_load_e - mean_load_m
, dif_sd   = sd_load_e - sd_load_m
)

pred.out[k, ] <- obj_max$dif_mean

if (((k / 10) %% 1) == 0) {
  print(paste("Through", (k/n_sim) * 100, "% of simulations"))
}

}

params_dataframe <- params$bd %>% dplyr::select(-c(name, tot_diff, good_fit)) %>% distinct() %>% 
  left_join(., params$trans) %>% 
  left_join(., params$seed) %>%
  left_join(., params$immunity %>% dplyr::select(-c(name, tot_diff, good_fit)) %>% distinct())

## Potentially a sensible way to explore good sets of parameters, but too many parameters for now
pred.out.gg <- cbind(
    params_dataframe
  , data.frame(fit_qual = pred.out %>% rowSums(na.rm = T))) %>%
  mutate(fit_qual = abs(fit_qual)) %>% 
  arrange(fit_qual) %>%
  mutate(good_fit = c(rep(1, 10), rep(0, n_sim - 10)) %>% as.factor()) 

#pred.out.gg %>% dplyr::select(-fit_qual) %>% {
#  ggpairs(., aes(colour = good_fit)) + 
#    scale_colour_brewer(palette = "Dark2")
#  }

####
## Example plot of simulated dynamics (for just the last parameter set at present)
## Could rework to save the best parameter set for example
####

I.l <- reshape2::melt(I)
names(I.l) <- c("Ind", "Day", "Infected")
I_load.l <- reshape2::melt(I_load)
names(I_load.l) <- c("Ind", "Day", "Load")
I.l <- I.l  %>% left_join(., I_load.l)

I.l %>% mutate(Load = ifelse(Load < 1, 1, Load)) %>% filter(Day < 240) %>% {
  ggplot(.) + geom_line(
   aes(Day, log(Load), group = Ind)
  , lwd = 0.50, colour = "firebrick3", alpha = 0.2) +
    geom_point(data = SB_bd, aes(JD, log_bd_load), alpha = 0.3) + 
    geom_line(data = SB_bd, aes(JD, log_bd_load, group = Mark), alpha = 0.5) +
    scale_x_continuous(breaks = c(25, 125, 225))
}

