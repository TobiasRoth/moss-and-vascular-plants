rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(ggthemes)
library(Rmisc)
library(arm)

# Plot settings
theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "none", 
      legend.background = element_rect(colour = "white"))
)

# Read and prepare data
dat <- read_csv("Data-raw/dat.csv") %>% 
  filter(HS == "alpine")
surveys <- read_csv("Data-raw/surveys.csv") %>% 
  filter(HS == "alpine")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Temporal trends in species richness ov Termo, Meso and Cryophilic species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bryophytes
glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = surveys, family = poisson) %>% summary
glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = surveys, family = poisson) %>% summary
glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = surveys, family = poisson) %>% summary

# Vascular plants
glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = surveys, family = poisson) %>% summary
glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = surveys, family = poisson) %>% summary
glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = surveys, family = poisson) %>% summary

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot: Short and long-lived species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat <- read_csv("Data-raw/dat.csv") 

#short-lived species
d.res_sh <- dat %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_mo_sh_trend, na.rm = TRUE),
    lo = t.test(T_mo_sh_trend)$conf.int[1],
    up = t.test(T_mo_sh_trend)$conf.int[2],
    gr = "short") 

#long-lived species  
d.res_lo <- dat %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_mo_lo_trend, na.rm = TRUE),
    lo = t.test(T_mo_lo_trend)$conf.int[1],
    up = t.test(T_mo_lo_trend)$conf.int[2],
    gr = "long")
d.res <- dplyr::bind_rows(d.res_sh, d.res_lo)
d.res <- d.res %>% mutate(
  HS = factor(HS, levels = c("colline", "montane", "subalpine", "alpine")),
  gr = factor(gr, levels = c("short", "long"))) 

#Graphics
ggplot(d.res, aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#00BFC4", "#C77CFF")) +
  labs(
    x = "",
    y = "Thermophilisation\n (temporal trend in T values per decade)",
    title = ""
  )
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

