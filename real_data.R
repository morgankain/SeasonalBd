####
## Load some real data
####

## Load data from Scotia Barrens and process it to extract just a few of the best sampled individuals
if (file.exists("SP_NOVI.csv") & !file.exists("SP_NOVI_processed.csv")) {
  
real_data <- read.csv("SP_NOVI.csv") %>% mutate(
  year = apply(CaptureDate %>% matrix(), 1, FUN = function(x) strsplit(x, "/")[[1]][3]) %>% as.numeric()
)

## Dates in R are annoying... Use a function that should fix dates with /
date_convert <- apply(matrix(real_data$CaptureDate), 1, FUN = function (x) {
  a <- strsplit(x, split = "/")[[1]]
  b <- a[3]
  b <- strsplit(b, "")[[1]]
  if (length(b) > 2) {
  b <- b[c(3, 4)] %>% paste(collapse = "")
  } else {
  b <- paste(b, collapse = "")
  }
  paste(c(a[c(1, 2)], b), collapse = "/")
})

real_data$date <- date_convert %>% as.Date(., "%m/%d/%y")
real_data      <- real_data %>% mutate(JD   = as.POSIXlt(date)$yday)  ## Get Julian Day for plotting

## Extract best measured and do some cleaning of temp data
real_data <- real_data %>% left_join(.
  , real_data %>% filter(!is.na(TargetCopies.swab)) %>% 
  group_by(IndividualID, year) %>% 
  summarize(n_meas = n()) %>% arrange(desc(n_meas))) %>%
  ungroup() %>% filter(n_meas == max(n_meas, na.rm = T)) %>%
  mutate(log_bd = log(TargetCopies.swab)) %>%
  left_join(., real_data %>% group_by(year, JD) %>% 
      summarize(temp = mean(Temp, na.rm = T)) %>% mutate(temp = round(temp, 1)))

## Save processed file for future runs
write.csv(real_data, "SP_NOVI_processed.csv")
  
## save a processed file because of public repo
} else if (file.exists("SP_NOVI_processed.csv")) {
  
real_data <- read.csv("SP_NOVI_processed.csv")
  
} else {
  print("Neither of the necessary files are present"); break
}
  
## Take a peek at these data
real_data %>% {
  ggplot(., aes(temp, TargetCopies.swab)) + 
    geom_point(aes(colour = IndividualID)) +
    geom_line(aes(colour = IndividualID)) +
    scale_y_log10() +
    xlab('Temperature') + ylab('Bd Load')
}

