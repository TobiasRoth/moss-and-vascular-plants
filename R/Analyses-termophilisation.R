rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(ggthemes)
library(Rmisc)
library(nlme)
library(arm)
library(lemon)

# Plot settings
theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.position = "none", 
      legend.background = element_rect(colour = "white"))
)

# Read and prepare data
dat <- read_csv("Data-raw/dat.csv")
surveys <- read_csv("Data-raw/surveys.csv") 

# Change in T value per m
moref <- -1 * coef(lm(T_mo_mean ~ elevation, data = dat))[2]
plref <- -1 * coef(lm(T_pl_mean ~ elevation, data = dat))[2]
tref <- 0.53 / (0.6 / 100)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Main figure ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

f_gettrend <- function(eleband, lu, sg) {
  # Settings
  nsim <- 1000
  sel <- ifelse(sg == "Vascular plants", "T_pl", "T_mo")
  startyear <- 2001
  endyear <- 2019
  
  # Data selection
  d <- surveys %>% 
    dplyr::filter(HS == paste(eleband) & land_use == paste(lu)) %>% 
    mutate(yr = year - 2010)
  d$T <- pull(d, sel)
  
  # Apply model
  predyear <- (startyear:endyear) - 2010
  mod <- lmer(T ~ yr + (1|aID_STAO), data = d)
  
  # Calculate and return results
  simres <- arm::sim(mod, nsim)@fixef %>% 
    apply(1, function(x) x[1] + x[2] * predyear)
  tibble(
    year = startyear: endyear,
    T = apply(simres, 1, mean),
    T_lo = apply(simres, 1, quantile, probs = 0.025),
    T_up = apply(simres, 1, quantile, probs = 0.975),
    eleband = eleband,
    lu = lu,
    sg = sg
  )
}

d <- map_dfr(
  c("colline", "montane", "subalpine", "alpine"), f_gettrend, lu = "grassland", sg = "Bryophytes") %>% 
  rbind(map_dfr(
    c("colline", "montane", "subalpine", "alpine"), f_gettrend, lu = "grassland", sg = "Vascular plants")) %>%
  rbind(map_dfr(
    c("alpine"), f_gettrend, lu = "unused", sg = "Bryophytes")) %>%
  rbind(map_dfr(
    c("alpine"), f_gettrend, lu = "unused", sg = "Vascular plants")) %>%
  rbind(map_dfr(
    c("colline", "montane", "subalpine"), f_gettrend, lu = "forest", sg = "Bryophytes")) %>%
  rbind(map_dfr(
    c("colline", "montane", "subalpine"), f_gettrend, lu = "forest", sg = "Vascular plants")) %>%
  mutate(eleband = factor(eleband, levels = rev(c("colline", "montane", "subalpine", "alpine"))))    

d %>% 
  ggplot(aes(x = year, y = T, ymin = T_lo, ymax = T_up, linetype = lu, col = lu, fill = lu)) +
  geom_line(lty = 1, lwd = 1) +
  geom_ribbon(alpha = 0.5, lty = "blank") +
  labs(x = "Study period", y = "Mean temperature value") +
  scale_fill_manual(values = c("#66C2A5", "#A6D854", "#B3B3B3")) +
  scale_color_manual(values = c("#66C2A5", "#A6D854", "#B3B3B3")) +
  facet_rep_grid(eleband ~ sg, scales = "free_y") +
  theme(legend.position="bottom")
ggsave("Figures/main-figure.pdf", height = 8, width = 5)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Notional elevational shifts ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Plot: temporal change across HS for forest plots
forest <-
  dat %>% 
  filter(land_use == "forest") %>% 
  filter(HS != "alpine") %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = median(T_pl_trend / plref, na.rm = TRUE),
    lo = t.test(T_pl_trend / plref)$conf.int[1],
    up = t.test(T_pl_trend/ plref)$conf.int[2],
    gr = "plants") %>% 
  rbind(
    dat %>% 
      filter(land_use == "forest") %>% 
      filter(HS != "alpine") %>% 
      group_by(HS) %>% 
      dplyr::summarise(
        mean = mean(T_mo_trend / moref, na.rm = TRUE),
        lo = t.test(T_mo_trend / moref)$conf.int[1],
        up = t.test(T_mo_trend / moref)$conf.int[2],
        gr = "moss")) %>% 
  ggplot(aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = tref, col = "grey60", lwd = 0.8)  +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#FF7F00", "#4DAF4A")) +
  # ylim(-300, 300) +
  labs(
    x = "",
    y = "Notional elevational shift\n(m per decade)",
    title = "(A) Forest") +
  scale_x_discrete(
    limits = c("colline", "montane", "subalpine"),
    labels = c("colline", "montane", "sub-\nalpine"))

# Plot: temporal change across HS for grassland plots
grassland <-
  dat %>% 
  filter(land_use == "grassland") %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_pl_trend / plref, na.rm = TRUE),
    lo = t.test(T_pl_trend / plref)$conf.int[1],
    up = t.test(T_pl_trend / plref)$conf.int[2],
    gr = "plants") %>% 
  rbind(
    dat %>% 
      filter(land_use == "grassland") %>% 
      group_by(HS) %>% 
      dplyr::summarise(
        mean = mean(T_mo_trend / moref, na.rm = TRUE),
        lo = t.test(T_mo_trend / moref)$conf.int[1],
        up = t.test(T_mo_trend / moref)$conf.int[2],
        gr = "moss")) %>% 
  ggplot(aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = tref, col = "grey60", lwd = 0.8)  +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#FF7F00", "#4DAF4A")) +
  # ylim(-300, 400) +
  labs(
    x = "",
    y = "Notional elevational shift\n(m per decade)",
    title = "(B) Grassland")  +
  scale_x_discrete(
    limits = c("colline", "montane", "subalpine", "alpine"),
    labels = c("colline", "montane", "sub-\nalpine", "alpine"))

# Plot: temporal change across HS for unused plots
unused <-
  dat %>% 
  filter(land_use == "unused") %>% 
  filter(HS == "alpine") %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    l = -1 * coef(lm(T_pl_mean ~ elevation))[2],
    mean = mean(T_pl_trend / plref, na.rm = TRUE),
    lo = t.test(T_pl_trend / plref)$conf.int[1],
    up = t.test(T_pl_trend / plref)$conf.int[2],
    gr = "plants") %>% 
  rbind(
    dat %>% 
      filter(land_use == "unused") %>% 
      filter(HS == "alpine") %>% 
      group_by(HS) %>% 
      dplyr::summarise(
        l = -1 * coef(lm(T_mo_mean ~ elevation))[2],
        mean = mean(T_mo_trend / moref, na.rm = TRUE),
        lo = t.test(T_mo_trend / moref)$conf.int[1],
        up = t.test(T_mo_trend / moref)$conf.int[2],
        gr = "moss")) %>% 
  ggplot(aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = tref, col = "grey60", lwd = 0.8)  +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#FF7F00", "#4DAF4A")) +
  # ylim(-300, 300) +
  labs(
    x = "",
    y = "Notional elevational shift\n(m per decade)",
    title = "(C) Unused") +
  scale_x_discrete(
    limits = c("alpine"),
    labels = c("alpine"))

pdf("Figures/Elevational-shifts.pdf", width = 10, height = 3.5)
multiplot(forest, grassland, unused, cols = 3)
dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Termophilisation accross all plots and among land-use types----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Across all plots
gls(
  T_mo_trend ~ 1, weights = varPower(form = ~ SR_mo_mean), 
  data = dat %>% filter(!is.na(T_mo_trend))) %>% 
  summary
gls(
  T_pl_trend ~ 1, weights = varPower(form = ~ SR_pl_mean), 
  data = dat %>% filter(!is.na(T_pl_trend))) %>% 
  summary

# Differences between land-use types
gls(
  T_mo_trend ~ ele + land_use, weights = varPower(form = ~ SR_mo_mean), 
  data = dat %>% filter(!is.na(T_mo_trend))) %>% 
  anova
gls(
  T_pl_trend ~ ele + land_use, weights = varPower(form = ~ SR_pl_mean), 
  data = dat %>% filter(!is.na(T_pl_trend))) %>% 
  anova

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Termophilisation accross elevation----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bryophytes
mod <- gls(
  T_mo_trend ~ ele, weights = varPower(form = ~ SR_mo_mean), 
  data = dat %>% filter(!is.na(T_mo_trend)))
summary(mod)
pr <- predict(mod, newdata = data.frame(ele = (c(500, 2000) -500) / 200)) %>% round(3)
round(pr / moref, 0)
round(pr / moref / tref, 3) 

# Vascular plants
mod <- gls(
  T_pl_trend ~ ele, weights = varPower(form = ~ SR_pl_mean), 
  data = dat %>% filter(!is.na(T_pl_trend)))
summary(mod)
pr <- predict(mod, newdata = data.frame(ele = (c(500, 2000) -500) / 200)) %>% round(3)
round(pr / plref, 0)
round(pr / plref / tref, 3) 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Difference between bryophytes and vascular plants ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare data
dd <- dat %>% 
  transmute(
    T_trend = T_mo_trend,
    SR = SR_mo_mean,
    ele = ele,
    HS = HS,
    aID_STAO = aID_STAO,
    bryo = 1
  ) %>% 
  rbind(
    dat %>% 
      transmute(
        T_trend = T_pl_trend,
        SR = SR_pl_mean,
        ele = ele,
        HS = HS,
        aID_STAO = aID_STAO,
        bryo = 0
      )
  ) %>% 
  filter(!is.na(T_trend))
lme(T_trend ~ bryo, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR), data = dd) %>% 
  anova
lme(T_trend ~ ele * bryo, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR), data = dd) %>% 
  summary
