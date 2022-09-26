####
## Plots of simulated relationships and Bd profiles
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
