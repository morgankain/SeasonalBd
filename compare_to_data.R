####
## Compare simulated bd profiles to a few of the best profiles obtained from the newts in PA
####

out.f <- out.f %>% left_join(.
  , real_data %>% dplyr::select(temp, TargetCopies.swab, IndividualID) %>% 
    pivot_wider(., names_from = IndividualID, values_from = TargetCopies.swab)
  )

## Just select a few individuals
best_sims <- out.f %>%
  mutate(
    XOXBXX = abs(log(Bd_load) - log(XOXBXX))^2
  , ROXXXR = abs(log(Bd_load) - log(ROXXXR))^2
  , XXRXXX = abs(log(Bd_load) - log(XXRXXX))^2
  , YYYYYY = abs(log(Bd_load) - log(YYYYYY))^2
    ) %>%
  pivot_longer(.,  -c(day, temp, gain, loss, bd_change, Bd_load, sim_num)) %>%
  group_by(sim_num, name) %>%
  summarize(tot_diff = sum(value, na.rm = T)) %>%
  arrange(tot_diff) %>% 
  filter(!is.infinite(tot_diff)) %>% 
  ungroup() %>%
  group_by(name) %>%
  mutate(good_fit = c(rep(1, 10), rep(0, n_sim - 10)))

out.f <- out.f %>% dplyr::select(-c(
  XOXBXX, ROXXXR, XXRXXX, YYYYYY
)) %>% left_join(., best_sims)

## Quick few plots for this comparison

gg.bd <- out.f %>% filter(good_fit == 1) %>% {
  ggplot(., aes(temp, gain)) + 
    geom_line(aes(group = sim_num), colour = "firebrick3", size = 0.5, alpha = 0.5) + 
    xlab("") + ylab("Bd growth") + facet_wrap(~name, nrow = 1)
}
  
gg.ir <- out.f %>% filter(good_fit == 1) %>% {
  ggplot(., aes(temp, loss)) + 
    geom_line(aes(group = sim_num), colour = "dodgerblue3", size = 0.5, alpha = 0.5) + 
    xlab("") + ylab("Immune 
Response") + facet_wrap(~name, nrow = 1)
}

gg.load <- out.f %>% filter(good_fit == 1) %>% 
  mutate(Bd_load = ifelse(Bd_load < 0.1, 0, Bd_load)) %>% {
  ggplot(., aes(temp, Bd_load)) + 
    geom_line(aes(group = sim_num), size = 0.5, alpha = 0.5) + 
    geom_point(data = real_data %>% rename(name = IndividualID) %>%
        filter(name %in% (out.f$name %>% unique()))
        , aes(temp, TargetCopies.swab), size = 1, colour = "dodgerblue3") +
    scale_y_log10() +
    xlab("Temparture") + ylab("Bd load") + facet_wrap(~name, nrow = 1)
}

grid.arrange(gg.bd, gg.ir, gg.load, ncol = 1)  

####
## Some exploration of the parameter space 
####

## Add in the "best" fits
params$immunity <- params$immunity %>% left_join(., best_sims)
params$bd       <- params$bd %>% left_join(., best_sims)

## Explore the parameter space of these best fits
library("GGally")

params$immunity %>% filter(name == "XOXBXX") %>%
  dplyr::select(A0, A1, A2, mult_t0, mult_t1, good_fit) %>%
  mutate(good_fit = as.factor(good_fit)) %>% {
  ggpairs(., aes(colour = good_fit)) + 
      scale_colour_brewer(palette = "Dark2")
  }
