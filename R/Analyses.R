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

# Read data
dat <- read_csv("Data-raw/dat.csv")
surveys <- read_csv("Data-raw/surveys.csv") 

# Change in T value per m
moref <- -1 * coef(lm(T_mo_mean ~ elevation, data = dat))[2]
plref <- -1 * coef(lm(T_pl_mean ~ elevation, data = dat))[2]
tref <- 0.53 / (0.6 / 100)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Termophilisation accross all plots ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gls(
  T_mo_trend ~ 1, weights = varPower(form = ~ SR_mo_mean), 
  data = dat %>% filter(!is.na(T_mo_trend))) %>% 
  summary
gls(
  T_pl_trend ~ 1, weights = varPower(form = ~ SR_pl_mean), 
  data = dat %>% filter(!is.na(T_pl_trend))) %>% 
  summary


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Main model on thermophilisation rate (Hyp: i- iii) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare data
d <- dat %>%
  dplyr::select(aID_STAO, ele, land_use, T_mo_trend, T_pl_trend) %>% 
  gather("vascpl", "thermo", -c(aID_STAO, ele, land_use)) %>% 
  mutate(vascpl = as.integer(factor(vascpl)) - 1) %>% 
  left_join(
    dat %>%
      dplyr::select(aID_STAO, SR_mo_mean, SR_pl_mean) %>% 
      gather("vascpl", "SR", -c(aID_STAO)) %>% 
      mutate(vascpl = as.integer(factor(vascpl)) - 1)) %>% 
  filter(!is.na(thermo)) %>% 
  mutate(land_use = factor(land_use, levels = c("grassland", "forest", "unused")))

# Apply main model
lme(thermo ~ vascpl + ele + land_use, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR), data = d) %>% 
  summary 

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
  scale_y_continuous(breaks = seq(0,5,0.1)) +
  labs(x = "Study period", y = "Temperature affinity of species communities") +
  scale_fill_manual(values = c("#66C2A5", "#A6D854", "#B3B3B3")) +
  scale_color_manual(values = c("#66C2A5", "#A6D854", "#B3B3B3")) +
  facet_rep_grid(eleband ~ sg, scales = "free_y", space = "free") +
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Temporal trends in alpine communities of Termo., Meso- and Cryophilic species numbers ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

d <- surveys %>% filter(HS == "alpine")

# Bryophytes
glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson) %>% summary
glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = d, family = poisson) %>% summary
glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = d, family = poisson) %>% summary

# Vascular plants
glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson) %>% summary
glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = d, family = poisson) %>% summary
glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = d, family = poisson) %>% summary

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Termophilisation between life strategies (Hyp iv)----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare data
d <- dat %>%
  dplyr::select(aID_STAO, ele, land_use, T_mo_sh_trend, T_mo_lo_trend) %>% 
  gather("strategy", "thermo", -c(aID_STAO, ele, land_use)) %>% 
  mutate(strategy = factor(strategy, levels = c("T_mo_sh_trend", "T_mo_lo_trend"))) %>% 
  left_join(dat %>% dplyr::select(aID_STAO, SR_mo_mean)) %>% 
  filter(!is.na(thermo)) %>% 
  mutate(land_use = factor(land_use, levels = c("grassland", "forest", "unused")))

# Apply main model
lme(thermo ~ strategy + ele + land_use, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR_mo_mean), data = d) %>% 
  summary 
lme(thermo ~ strategy, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR_mo_mean), data = d) %>% 
  summary 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot: Short and long-lived species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#short-lived species
d.res_sh <- dat %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_mo_sh_trend, na.rm = TRUE),
    lo = t.test(T_mo_sh_trend)$conf.int[1],
    up = t.test(T_mo_sh_trend)$conf.int[2],
    gr = "short-lived species") %>% 
  rbind(
    tibble(
      HS = "overall",
      mean = mean(dat$T_mo_sh_trend, na.rm = TRUE),
      lo = t.test(dat$T_mo_sh_trend)$conf.int[1],
      up = t.test(dat$T_mo_sh_trend)$conf.int[2],
      gr = "short-lived species")
  )

#long-lived species  
d.res_lo <- dat %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_mo_lo_trend, na.rm = TRUE),
    lo = t.test(T_mo_lo_trend)$conf.int[1],
    up = t.test(T_mo_lo_trend)$conf.int[2],
    gr = "long-lived species")%>% 
  rbind(
    tibble(
      HS = "overall",
      mean = mean(dat$T_mo_lo_trend, na.rm = TRUE),
      lo = t.test(dat$T_mo_lo_trend)$conf.int[1],
      up = t.test(dat$T_mo_lo_trend)$conf.int[2],
      gr = "long-lived species")
  )

# Combine results of short- and long-lived spcies
d.res <- dplyr::bind_rows(d.res_sh, d.res_lo)
d.res <- d.res %>% mutate(
  HS = factor(HS, levels = c("overall", "colline", "montane", "subalpine", "alpine")),
  gr = factor(gr, levels = c("short-lived species", "long-lived species"))) 

#Graphics
ggplot(d.res, aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25), cex = 1.7) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#00BFC4", "#C77CFF")) +
  labs(
    x = "",
    y = "Thermophilisation rate",
    title = ""
  ) +
  theme(legend.position="bottom")
ggsave("Figures/Thermophilisation-short-long-lived.pdf", height = 5, width = 8)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Linear models: Short and long-lived species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare data
dd <- dat %>% 
  transmute(
    T_trend = T_mo_sh_trend,
    SR = SR_mo_mean,
    ele = ele,
    HS = HS,
    aID_STAO = aID_STAO,
    short = 1
  ) %>% 
  rbind(
    dat %>% 
      transmute(
        T_trend = T_mo_lo_trend,
        SR = SR_mo_mean,
        ele = ele,
        HS = HS,
        aID_STAO = aID_STAO,
        short = 0
      )
  ) %>% 
  filter(!is.na(T_trend))

# Apply models
lme(T_trend ~ short, random = ~ 1 | aID_STAO, data = dd) %>% 
  summary
lme(T_trend ~ short, random = ~ 1 | aID_STAO, 
    data = dd %>% filter(HS == "alpine")) %>% 
  anova

# Notional elevational shifts
shref <- -1 * coef(lm(T_mo_sh_mean ~ elevation, data = dat))[2]
loref <- -1 * coef(lm(T_mo_lo_mean ~ elevation, data = dat))[2]
moref <- -1 * coef(lm(T_mo_mean ~ elevation, data = dat))[2]
d.res_sh[d.res_sh$HS == "alpine", "mean"] / moref
d.res_lo[d.res_lo$HS == "alpine", "mean"] / moref