###############################################################################
## Attempting to explain patterns of Bd load within individuals through time ##
###############################################################################

####
## Notes
####

## 1) These scripts do three things:
 ## A) Simulate within-individual Bd load progression based on temperature and time since infection
   ##   as a combiniation of Bd growth rate and animal immune response
 ## B) Explore variation in parameters controlling bd growth and immune response and comapre
   ##   these estimated profiles to some measured profiles from Scotia Barrens
 ## C) Run a simple SI model with transmission based on the distribution of load in the environment 
   ## (essentially a rough start to an IPM)
    ## NOTE: only just started working on this (note made Sep 26) so this is likely quite buggy

## To use:
 ## (A and B from above) Open "parameters.R" and set up desired parameters. Run through this script top to bottom
  ## Can set uncertainty in Bd growth and/or Immune response on or off
   ## i)   Turn uncertainty off for both to explore/plot single relationships
   ## ii)  Setting uncertainty on for just Immune response seems like a --first pass-- sensible way to explore what a population of 
    ##      bd loads may look like given variation among individuals in immune function
   ## iii) Setting both on seems like a --first pass-- way of generally exploring how variable Bd loads may look given
    ##      expected uncertainty in these functional forms (which could be hard to define TBH)
 ## (C from above) Do the above and then open "epi_sim.R" and adjust parameters and then run through that script
  ## Can set uncertainty in transmission and initiating infection on/off. Similar notes to ^^

## Currently uncertainty is captured as a range and a Sobol sequence over the range of all parameters
 ## Not the most sensible moving forward (will want to set up some correlation matrix among parameters)

####
## Packages and Functions
####
source("packages_and_functions.R")

####
## Load some real data
####
source("real_data.R")

####
## Parameters
####
source("parameters.R")

####
## Establish Bd Growth + Immunity vs Temperature
##   and
## Predict Bd Profiles from ^^
####
source("pred_bd.R")

####
## Plots of simulated relationships and Bd profiles
####
source("plots.R")

####
## Compare to "real data"
####
source("compare_to_data.R")

####
## Try a simple SIR-style transmission model with load dependent transmission
## (essentially a custom IPM)
####
uncer.init  <- TRUE
uncer.trans <- TRUE
 ## OPEN to choose parameters, do not just source
source("epi_sim.R")



