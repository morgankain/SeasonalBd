#############################################################################
## Toy / Initial sketch of a model to explain patterns of Bd in the field, ##
## scaling up from within- to between-individual processes                 ##
#############################################################################

####
## Notes
####

## 1) These scripts do three things:
 ## A) Simulate within-individual Bd load progression based on temperature and time since infection
   ##   as a combination of Bd growth rate and animal immune response
 ## B) Explore variation in parameters controlling bd growth and immune response and compare
   ##   these estimated profiles to some measured profiles from Scotia Barrens, PA
 ## C) Run a simple SI model with transmission based on the distribution of load in the environment 
   ## (essentially a rough start to an Integral Projection Model [IPM])
    ## NOTE: only just started working on this (i.e., 2-3 days or so of work) so this epidemic simulation is potentially quite buggy

## 2) To use these scripts:
 ## (A and B from above) Open "parameters.R" and set up desired parameters. Run through this script top to bottom
  ## Can set uncertainty in Bd growth and/or Immune response on or off
   ## i)   Turn uncertainty off for both to explore/plot single relationships
   ## ii)  Setting uncertainty on for just Immune response as a potential --first pass-- sensible way to explore what a population of 
    ##      bd loads may look like given variation among individuals in immune function
   ## iii) Setting both on seems like a --first pass-- way of generally exploring how variable Bd loads may look given
    ##      expected uncertainty in these functional forms (which could be hard to define TBH)
 ## (C from above) Do the above and then open "epi_sim.R" and adjust parameters and then run through that script
  ## Can set uncertainty in transmission and initiating infection on/off. Similar notes to ^^

## Currently uncertainty is captured as a range and a Sobol sequence over the range of all parameters
 ## Not the most sensible moving forward (will want to set up some correlation matrix among parameters,
  ## especially for within-host processes once some models are fit to data)


####
## Code
####

## Packages and Functions
source("packages_and_functions.R")

## Load some real data
source("real_data.R")

## Parameters
source("parameters.R")

## Establish Bd Growth + Immunity vs Temperature and Predict Bd Profiles using the parameters in the above script
source("pred_bd.R")

## Plots of simulated relationships and Bd profiles
source("plots.R")

## Compare to "real data"
source("compare_to_data.R")

## Try a simple SIR-style transmission model with load dependent transmission (essentially a custom IPM)
 ## NOTE: open to set parameters, do not just source (will run with default parameters)
  ## NOTE: more summarizing still needed to explore epidemic outcomes as a function of the parameter set;
   ##      haven't done this yet, but see my github on COVID for some strategies for this
uncer.init  <- TRUE  ## initial state uncertainty
uncer.trans <- TRUE  ## transmission uncertainty
source("epi_sim.R")

