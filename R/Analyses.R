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
library(broom)
library(knitr)
library(kableExtra)
library(lmerTest)
library(patchwork)

# Plot settings
theme_set(
  theme_clean() +
    theme(
      plot.background = element_rect(color = "white"),
      plot.title = element_text(face = "plain", hjust = 0.5),
      legend.title = element_blank(), 
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.position = "bottom",
      legend.background = element_rect(colour = "white"))
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Read and prepare data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read data
dat <- read_csv("Data-raw/dat.csv")
surveys <- read_csv("Data-raw/surveys.csv") 

# Transform elevation 
dat <- dat %>% 
  mutate(
    ele = (elevation - mean(dat$elevation[dat$HS == "colline"])) / 100
  )

# Plot T value against elevation
plmo <- dat %>% 
  ggplot(aes(x = elevation, y = T_mo_mean)) +
  geom_point(cex = 0.6) +
  geom_smooth(method = lm) +
  labs(x = "Elevation [m]", y = "Community temperature index (CTI)", title = "Bryophytes") +
  ylim(1,5)
lm(T_mo_mean ~ elevation, data =  dat) %>% summary
plpl <- dat %>% 
  ggplot(aes(x = elevation, y = T_pl_mean)) +
  geom_point(cex = 0.6) +
  geom_smooth(method = lm) +
  labs(x = "Elevation [m]", y = "Community temperature index (CTI)", title = "Vascular plants") +
  ylim(1,5)
lm(T_pl_mean ~ elevation, data =  dat) %>% summary
pdf("Figures/Temperature_affinities_elevation_shift.pdf", width = 10, height = 3.5)
multiplot(plmo, plpl, cols = 2)
dev.off()

# Change in T value per m (for notional shift)
mod <- lm(T_mo_mean ~ elevation, data = dat)
summary(mod)
moref <- -1 * coef(mod)[2]
mod <- lm(T_pl_mean ~ elevation, data = dat)
summary(mod)
plref <- -1 * coef(mod)[2]
tref <- 63:84

# Change Termophilisation to notional shift
dat <- dat %>% 
  mutate(
    T_mo_No_shift = T_mo_trend / moref,
    T_pl_No_shift = T_pl_trend / plref,
    T_sh_Pl_No_shift = T_pl_sh_trend / plref,
    T_lo_Pl_No_shift = T_pl_lo_trend / plref,
    T_sh_No_shift = T_mo_sh_trend / moref,
    T_lo_No_shift = T_mo_lo_trend / moref
  )

# Number of sites
n_distinct(dat$aID_STAO)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Table with sample size (Table 1)----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Elevational zones and land use types
dat %>% 
  group_by(`Elevational zone` = HS, `Land use type` = land_use) %>%
  dplyr::summarise(
    `Nplots` = n(),
    `Elevation (m)` = mean(elevation) %>% round(0),
    `N trend bryophytes` = sum(!is.na(T_mo_trend)),
    `Mean NES bryo (m)` = mean(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES bryo (m)` = sd(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `N trend vascular plants` = sum(!is.na(T_pl_trend)),
    `Mean NES pl (m)` = mean(T_pl_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES pl (m)` = sd(T_pl_No_shift, na.rm = TRUE) %>% round(1)
  )  %>% 
  mutate(`Elevational zone` = factor(`Elevational zone`, levels = c("colline", "montane", "subalpine", "alpine", "overall"))) %>% 
  mutate(`Land use type` = factor(`Land use type`, levels = c("grassland", "forest", "unused"))) %>% 
  arrange(`Elevational zone`, `Land use type`) %>% 
  as.data.frame()

# Elevational zones
dat %>% 
  group_by(`Elevational zone` = HS) %>%
  dplyr::summarise(
    `Nplots` = n(),
    `Elevation (m)` = mean(elevation) %>% round(0),
    `N trend bryophytes` = sum(!is.na(T_mo_trend)),
    `Mean NES bryo (m)` = mean(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES bryo (m)` = sd(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `N trend vascular plants` = sum(!is.na(T_pl_trend)),
    `Mean NES pl (m)` = mean(T_pl_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES pl (m)` = sd(T_pl_No_shift, na.rm = TRUE) %>% round(1)
  )  %>% 
  mutate(`Elevational zone` = factor(`Elevational zone`, levels = c("colline", "montane", "subalpine", "alpine", "overall"))) %>% 
  arrange(`Elevational zone`) %>% 
  as.data.frame()

# land use types
dat %>% 
  group_by(`Land use type` = land_use) %>%
  dplyr::summarise(
    `Nplots` = n(),
    `Elevation (m)` = mean(elevation) %>% round(0),
    `N trend bryophytes` = sum(!is.na(T_mo_trend)),
    `Mean NES bryo (m)` = mean(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES bryo (m)` = sd(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `N trend vascular plants` = sum(!is.na(T_pl_trend)),
    `Mean NES pl (m)` = mean(T_pl_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES pl (m)` = sd(T_pl_No_shift, na.rm = TRUE) %>% round(1)
  )  %>% 
  mutate(`Land use type` = factor(`Land use type`, levels = c("grassland", "forest", "unused"))) %>% 
  arrange(`Land use type`) %>% 
  as.data.frame()

# all
dat %>% 
  dplyr::summarise(
    `Nplots` = n(),
    `Elevation (m)` = mean(elevation) %>% round(0),
    `N trend bryophytes` = sum(!is.na(T_mo_trend)),
    `Mean NES bryo (m)` = mean(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES bryo (m)` = sd(T_mo_No_shift, na.rm = TRUE) %>% round(1),
    `N trend vascular plants` = sum(!is.na(T_pl_trend)),
    `Mean NES pl (m)` = mean(T_pl_No_shift, na.rm = TRUE) %>% round(1),
    `SD NES pl (m)` = sd(T_pl_No_shift, na.rm = TRUE) %>% round(1)
  )  %>% 
  as.data.frame()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Table with descriptive statistics (Table S3)----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat %>% 
  group_by(`Elevational zone` = HS, `Land use type` = land_use) %>%
  dplyr::summarise(
    `Species richness bryophytes` = paste0(round(mean(SR_mo_mean), 1), " ± ", round(sd(SR_mo_mean), 1)),
    `Tempera-ture affinity bryophytes` = paste0(round(mean(T_mo_mean, na.rm = TRUE), 2), " ± ", round(sd(T_mo_mean, na.rm = TRUE), 2)),
    `Species richness short lived bryophytes` = paste0(round(mean(SR_mo_sh_mean), 1), " ± ", round(sd(SR_mo_sh_mean), 1)),
    `Species richness long lived bryophytes` = paste0(round(mean(SR_mo_lo_mean), 1), " ± ", round(sd(SR_mo_lo_mean), 1)),
    `Species richness vascular plants` = paste0(round(mean(SR_pl_mean), 1), " ± ", round(sd(SR_pl_mean), 1)),
    `Tempera-ture affinity vascular plants` = paste0(round(mean(T_pl_mean, na.rm = TRUE), 2), " ± ", round(sd(T_pl_mean, na.rm = TRUE), 2)),
    `Species richness short lived vascular plants` = paste0(round(mean(SR_pl_sh_mean), 1), " ± ", round(sd(SR_pl_sh_mean), 1)),
    `Species richness long lived vascular plants` = paste0(round(mean(SR_pl_lo_mean), 1), " ± ", round(sd(SR_pl_lo_mean), 1)),
  ) %>% 
  rbind(
    tibble(
      `Species richness bryophytes` = paste0(round(mean(dat$SR_mo_mean), 1), " ± ", round(sd(dat$SR_mo_mean), 1)),
      `Species richness short lived bryophytes` = paste0(round(mean(dat$SR_mo_sh_mean, na.rm = TRUE), 1), " ± ", round(sd(dat$SR_mo_sh_mean), 1)),
      `Species richness long lived bryophytes` = paste0(round(mean(dat$SR_mo_lo_mean, na.rm = TRUE), 1), " ± ", round(sd(dat$SR_mo_lo_mean), 1)),
      `Tempera-ture affinity bryophytes` = paste0(round(mean(dat$T_mo_mean, na.rm = TRUE), 2), " ± ", round(sd(dat$T_mo_mean, na.rm = TRUE), 2)),
      `Species richness vascular plants` = paste0(round(mean(dat$SR_pl_mean), 1), " ± ", round(sd(dat$SR_pl_mean), 1)),
      `Tempera-ture affinity vascular plants` = paste0(round(mean(dat$T_pl_mean, na.rm = TRUE), 2), " ± ", round(sd(dat$T_pl_mean, na.rm = TRUE), 2)),
      `Species richness short lived vascular plants` = paste0(round(mean(dat$SR_pl_sh_mean, na.rm = TRUE), 1), " ± ", round(sd(dat$SR_pl_sh_mean), 1)),
      `Species richness long lived vascular plants` = paste0(round(mean(dat$SR_pl_lo_mean, na.rm = TRUE), 1), " ± ", round(sd(dat$SR_pl_lo_mean), 1)),
    ) 
  ) %>% 
  mutate(`Elevational zone` = factor(`Elevational zone`, levels = c("colline", "montane", "subalpine", "alpine", "overall"))) %>% 
  arrange(`Elevational zone`) %>% 
  kable(
    digits = 0,
    align = c("l", "l", rep("r", 4)),
    booktabs = T) %>% 
  kable_styling()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Average trend models: Termophilisation and notianal shift accross all plots ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Termophilisation
gls(
  T_mo_trend ~ 1, weights = varPower(form = ~ SR_mo_mean), 
  data = dat %>% filter(!is.na(T_mo_trend))) %>% 
  summary
gls(
  T_pl_trend ~ 1, weights = varPower(form = ~ SR_pl_mean), 
  data = dat %>% filter(!is.na(T_pl_trend))) %>% 
  summary

# Notional shift
gls(
  T_mo_No_shift ~ 1, weights = varPower(form = ~ SR_mo_mean), 
  data = dat %>% filter(!is.na(T_mo_trend))) %>% 
  summary
gls(
  T_pl_No_shift ~ 1, weights = varPower(form = ~ SR_pl_mean), 
  data = dat %>% filter(!is.na(T_pl_trend))) %>% 
  summary

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Full model, Table 2: Notional elevation shift between species groups, land use types and across elevation ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare data
d <- dat %>%
  dplyr::select(aID_STAO, ele, land_use, HS, T_mo_No_shift, T_pl_No_shift) %>% 
  gather("vascpl", "thermo", -c(aID_STAO, ele, land_use, HS)) %>% 
  mutate(vascpl = as.integer(factor(vascpl)) - 1) %>% 
  left_join(
    dat %>%
      dplyr::select(aID_STAO, SR_mo_mean, SR_pl_mean) %>% 
      gather("vascpl", "SR", -c(aID_STAO)) %>% 
      mutate(vascpl = as.integer(factor(vascpl)) - 1)) %>% 
  filter(!is.na(thermo)) %>% 
  mutate(
    land_use = factor(land_use, levels = c("grassland", "forest", "unused")),
    HS = factor(HS, levels = c("colline", "montane", "subalpine", "alpine")))

# Apply linneage model
mod <- lme(thermo ~ vascpl, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR), data = d)
anova(mod)
summary(mod)$tTable[, c("Value", "Std.Error", "p-value")] %>%  
  kable(
    digits = c(2, 2, 3),
    align = "l",
    booktabs = T) %>% 
  kable_styling()

# Apply main model
mod <- lme(thermo ~ vascpl * ele * land_use, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR), data = d)
anova(mod)
summary(mod)$tTable[, c("Value", "Std.Error", "p-value")] %>%  
  kable(
    digits = c(2, 2, 3),
    align = "l",
    booktabs = T) %>% 
  kable_styling()

mod <- lme(thermo ~ vascpl + ele + land_use, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR), data = d)
anova(mod)
summary(mod)$tTable[, c("Value", "Std.Error", "p-value")] %>%  
  kable(
    digits = c(2, 2, 3),
    align = "l",
    booktabs = T) %>% 
  kable_styling()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure 2: Results from full model without non-significant interactions ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get bootstrap sample
getbootsample <- function(s) {
  newdat <- dsel[sample(1:nrow(dsel), replace = TRUE),]
  out <- newdat %>% 
    mutate(pre = predict(mod, newdata = newdat)) %>% 
    group_by(HS, land_use, vascpl) %>% 
    dplyr::summarise(No_shift = mean(thermo)) 
  names(out)[4] <- paste0("S_", s)
  out
}

# Select data for all land use types and species groups with enough data
dsel <- d %>% 
  filter(!(HS == "colline" & land_use == "unused")) %>% 
  filter(!(HS == "montane" & land_use == "unused")) %>% 
  filter(!(HS == "subalpine" & land_use == "unused")) %>% 
  filter(!(HS == "alpine" & land_use == "forest")) 
  
# Make predictions using bootstrap samples
res <- dsel %>% 
  group_by(HS, land_use, vascpl) %>% 
  dplyr::summarise(No_shift = mean(thermo)) 
for(s in 1:1000){
  res <- res %>% left_join(getbootsample(s))
}
pred <- res[, 1:4] %>% 
  mutate(
    land_use = fct_recode(land_use, `unmanaged open areas` = "unused"),
    vascpl = factor(vascpl, levels = c(0, 1), labels = c("Bryophytes", "Vascular plants")),
    av = apply(res[, 5:ncol(res)], 1, mean),
    lo = apply(res[, 5:ncol(res)], 1, quantile, probs = 0.025),
    up = apply(res[, 5:ncol(res)], 1, quantile, probs = 0.975)
  ) %>% 
  mutate(
    land_use = factor(land_use, levels = c("forest", "grassland", "unmanaged open areas"))
  )

# Make figure with predictions for each land-use type and elevational band
forest <-pred %>% 
  filter(land_use == "forest") %>% 
  ggplot(aes(y = No_shift, x = HS, col = vascpl, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = tref, col = "grey80", lwd = 0.8)  +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#FF7F00", "#4DAF4A")) +
  ylim(-220, 420) +
  labs(
    x = "",
    y = "Notional elevation shift (NES)\n[m per decade]",
    title = "Forests") +
  scale_x_discrete(
    limits = c("colline", "montane", "subalpine"),
    labels = c("Colline", "Montane", "Sub-\nalpine"))
grassland <-pred %>% 
  filter(land_use == "grassland") %>% 
  ggplot(aes(y = No_shift, x = HS, col = vascpl, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = tref, col = "grey80", lwd = 0.8)  +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#FF7F00", "#4DAF4A")) +
  ylim(-220, 420) +
  labs(
    x = "",
    y = "Notional elevation shift (NES)\n[m per decade]",
    title = "Managed grasslands") +
  scale_x_discrete(
    limits = c("colline", "montane", "subalpine", "alpine"),
    labels = c("Colline", "Montane", "Sub-\nalpine", "Alpine")) 
unused <-pred %>% 
  filter(land_use == "unmanaged open areas") %>% 
  ggplot(aes(y = No_shift, x = HS, col = vascpl, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = tref, col = "grey80", lwd = 0.8)  +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(width = 0.075, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#FF7F00", "#4DAF4A")) +
  ylim(-220, 420) +
  labs(
    x = "",
    y = "Notional elevation shift (NES)\n[m per decade]",
    title = "Unmanaged open areas") +
  scale_x_discrete(
    limits = c("alpine"),
    labels = c("Alpine"))

grassland + forest + unused + plot_layout(guides = "collect")
ggsave("Figures/Notional_elevation_shift.pdf", width = 10, height = 4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Table S4: notional shift for elevational zones ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat %>%
  group_by(HS) %>%
  dplyr::summarise(
    shift_bryo = mean(T_mo_No_shift, na.rm = TRUE),
    lo_bryo = t.test(T_mo_No_shift)$conf.int[1],
    up_bryo = t.test(T_mo_No_shift)$conf.int[2]) %>%
  left_join(
    dat %>%
      group_by(HS) %>%
      dplyr::summarise(
        shift_pl = mean(T_pl_No_shift, na.rm = TRUE),
        lo_pl = t.test(T_pl_No_shift)$conf.int[1],
        up_pl = t.test(T_pl_No_shift)$conf.int[2])
  ) %>%
  mutate(
    HS = factor(HS, levels = c("colline", "montane", "subalpine", "alpine"))) %>%
  arrange(HS) %>%
  kable(
    digits = 1,
    align = c("l", "l", rep("r", 6)),
    booktabs = T) %>%
  kable_styling()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Main figure ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

getpvalue <- function(x) {
  res <- paste("p =", format(round(x, 2), nsmall = 3))
  if(res == "p = 0.000") res <- "p < 0.001"
  res
}

f_gettrend <- function(eleband, lu, sg) {
  # Settings
  nsim <- 1000
  sel <- ifelse(sg == "Vascular plants", "T_pl", "T_mo")
  startyear <- 2001
  endyear <- 2021
  
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
    sg = sg,
    pvalue = summary(mod)$coefficient[2, 5] %>% getpvalue
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
    c("colline", "montane", "subalpine"), f_gettrend, lu = "forest", sg = "Vascular plants")) 
d$lu[d$lu == "unused"] <- "Unmanaged open areas"
d$lu[d$lu == "grassland"] <- "Managed grasslands"
d$lu[d$lu == "forest"] <- "Forests"
d$eleband[d$eleband == "colline"] <- "Colline"
d$eleband[d$eleband == "montane"] <- "Montane"
d$eleband[d$eleband == "subalpine"] <- "Subalpine"
d$eleband[d$eleband == "alpine"] <- "Alpine"
d <- d %>% 
  mutate(
    eleband = factor(eleband, levels = rev(c("Colline", "Montane", "Subalpine", "Alpine"))),
    lu = factor(lu, levels = c("Managed grasslands", "Forests", "Unmanaged open areas"))
  )
dd <- d %>% 
  group_by(eleband, lu, sg) %>%
  dplyr::summarize(
    T = T[year == 2019] + 0.02,
    pwert = first(pvalue), 
    T_lo = as.numeric(NA), 
    T_up = as.numeric(NA)
  )
dd$T[dd$lu == "Managed grasslands" & dd$sg == "Bryophytes" & dd$eleband == "Colline"] <- 3.44

d %>% 
  ggplot(aes(x = year, y = T, ymin = T_lo, ymax = T_up, linetype = lu, col = lu, fill = lu)) +
  geom_line(lty = 1, lwd = 1) +
  geom_ribbon(alpha = 0.3, lty = "blank") +
  scale_y_continuous(breaks = seq(0,5,0.1)) +
  labs(x = "Year", y = "Community temperature index (CTI)") +
  scale_fill_manual(values = c("#A6D854", "#66C2A5", "#B3B3B3")) +
  scale_color_manual(values = c("#A6D854", "#66C2A5", "#B3B3B3")) +
  facet_rep_grid(eleband ~ sg, scales = "free_y", space = "free") + 
  geom_text(aes(x = 2017, label = pwert), data = dd, size = 2.5, fontface = 2) +
  # geom_text(aes(x = 2017, label = pwert), data = dd, col = "black", alpha = 0.4, size = 2.5, fontface = 2) +
  theme(legend.position="bottom") 
ggsave("Figures/main-figure.pdf", height = 8, width = 5.3)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Table 3, Species richness models: Temporal trends in termo., meso- and cryophilic species numbers ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Table with number of species in the three groups
surveys %>% 
  group_by(`Elevational zone` = HS) %>%
  dplyr::summarise(
    `SR cryo bryophytes` = paste0(round(mean(AZ_mo_Cryo), 1), " ± ", round(sd(AZ_mo_Cryo), 1)),
    `SR meso bryophytes` = paste0(round(mean(AZ_mo_Meso), 1), " ± ", round(sd(AZ_mo_Meso), 1)),
    `SR termo bryophytes` = paste0(round(mean(AZ_mo_Termo), 1), " ± ", round(sd(AZ_mo_Termo), 1)),
    `SR cryo vasc plants` = paste0(round(mean(AZ_pl_Cryo), 1), " ± ", round(sd(AZ_pl_Cryo), 1)),
    `SR meso vasc plants` = paste0(round(mean(AZ_pl_Meso), 1), " ± ", round(sd(AZ_pl_Meso), 1)),
    `SR termo vasc plants` = paste0(round(mean(AZ_pl_Termo), 1), " ± ", round(sd(AZ_pl_Termo), 1))
  ) %>% 
  mutate(`Elevational zone` = factor(`Elevational zone`, levels = c("colline", "montane", "subalpine", "alpine", "overall"))) %>% 
  arrange(`Elevational zone`) %>% 
  kable(
    digits = 0,
    align = c("l", rep("r", 6)),
    booktabs = T) %>% 
  kable_styling()


# a) Total
d <- surveys 
rbind(
  summary(glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()
rbind(
  summary(glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()

# b) Colline
d <- surveys  %>% filter(HS == "colline")
rbind(
  summary(glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()
rbind(
  summary(glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()

# c) montane
d <- surveys  %>% filter(HS == "montane")
rbind(
  summary(glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()
rbind(
  summary(glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()

# d) subalpine
d <- surveys  %>% filter(HS == "subalpine")
rbind(
  summary(glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()
rbind(
  summary(glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()

# d) alpine
d <- surveys  %>% filter(HS == "alpine")
rbind(
  summary(glmer(AZ_mo_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_mo_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()
rbind(
  summary(glmer(AZ_pl_Cryo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Meso ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)],
  summary(glmer(AZ_pl_Termo ~ yr + (1|aID_STAO), data = d, family = poisson))$coefficients[2, c(1, 2, 4)]) %>% 
  kable(digits = 3, align = "l", booktabs = T) %>% 
  kable_styling()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Life-strategy models (Table S5): Notional elevational shift between life strategies ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Model for bryophytes
d <- dat %>%
  dplyr::select(aID_STAO, ele, land_use, T_sh_No_shift, T_lo_No_shift) %>% 
  gather("strategy", "thermo", -c(aID_STAO, ele, land_use)) %>% 
  mutate(strategy = factor(strategy, levels = c("T_sh_No_shift", "T_lo_No_shift"))) %>% 
  left_join(dat %>% dplyr::select(aID_STAO, SR_mo_mean)) %>% 
  filter(!is.na(thermo)) %>% 
  mutate(land_use = factor(land_use, levels = c("grassland", "forest", "unused")))
mod <- lme(thermo ~ strategy + ele + land_use, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR_mo_mean), data = d) 
summary(mod)$tTable[, c("Value", "Std.Error", "p-value")] %>%  
  kable(
    digits = c(2, 2, 3),
    align = "l",
    booktabs = T) %>% 
  kable_styling()

# Model for vascular plants
d <- dat %>%
  dplyr::select(aID_STAO, ele, land_use, T_sh_Pl_No_shift, T_lo_Pl_No_shift) %>% 
  gather("strategy", "thermo", -c(aID_STAO, ele, land_use)) %>% 
  mutate(strategy = factor(strategy, levels = c("T_sh_Pl_No_shift", "T_lo_Pl_No_shift"))) %>% 
  left_join(dat %>% dplyr::select(aID_STAO, SR_pl_mean)) %>% 
  filter(!is.na(thermo)) %>% 
  mutate(land_use = factor(land_use, levels = c("grassland", "forest", "unused")))
mod <- lme(thermo ~ strategy + ele + land_use, random = ~ 1 | aID_STAO, weights = varPower(form = ~ SR_pl_mean), data = d) 
summary(mod)$tTable[, c("Value", "Std.Error", "p-value")] %>%  
  kable(
    digits = c(2, 2, 3),
    align = "l",
    booktabs = T) %>% 
  kable_styling()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot: Notial elevation shift of short and long-lived species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#short-lived species
d <- dat
d$HS[d$HS == "colline"] <- "Colline"
d$HS[d$HS == "montane"] <- "Montane"
d$HS[d$HS == "subalpine"] <- "Subalpine"
d$HS[d$HS == "alpine"] <- "Alpine"

d.res_sh <- d %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_sh_No_shift, na.rm = TRUE),
    lo = t.test(T_sh_No_shift)$conf.int[1],
    up = t.test(T_sh_No_shift)$conf.int[2],
    gr = "Short-lived") %>% 
  rbind(
    tibble(
      HS = "All zones",
      mean = mean(dat$T_sh_No_shift, na.rm = TRUE),
      lo = t.test(dat$T_sh_No_shift)$conf.int[1],
      up = t.test(dat$T_sh_No_shift)$conf.int[2],
      gr = "Short-lived")
  )

#long-lived species  
d.res_lo <- d %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    mean = mean(T_lo_No_shift, na.rm = TRUE),
    lo = t.test(T_lo_No_shift)$conf.int[1],
    up = t.test(T_lo_No_shift)$conf.int[2],
    gr = "Long-lived")%>% 
  rbind(
    tibble(
      HS = "All zones",
      mean = mean(dat$T_lo_No_shift, na.rm = TRUE),
      lo = t.test(dat$T_lo_No_shift)$conf.int[1],
      up = t.test(dat$T_lo_No_shift)$conf.int[2],
      gr = "Long-lived")
  )

# Combine results of short- and long-lived spcies
d.res <- dplyr::bind_rows(d.res_sh, d.res_lo)
d.res <- d.res %>% mutate(
  HS = factor(HS, levels = c("All zones", "Colline", "Montane", "Subalpine", "Alpine")),
  gr = factor(gr, levels = c("Short-lived", "Long-lived"))) 

# Graphics
ggplot(d.res, aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = tref, col = "grey80", lwd = 0.8)  +
  geom_point(position = position_dodge(width = 0.25), cex = 1.7) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#00BFC4", "#C77CFF")) +
  labs(
    x = "",
    y = "Notional elevation shift (NES)\n[m per decade]",
    title = ""
  ) +
  theme(legend.position="bottom")
ggsave("Figures/Thermophilisation-short-long-lived.pdf", height = 4, width = 6)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot: Notial elevation shift of short and long-lived bryophytes: differnces between land use types ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#short-lived species
d <- dat
d$land_use[d$land_use == "forest"] <- "Forests"
d$land_use[d$land_use == "grassland"] <- "Managed\ngrasslands"
d$land_use[d$land_use == "unused"] <- "Unmanaged\nopen areas"

d.res_sh <- d %>% 
  group_by(land_use) %>% 
  dplyr::summarise(
    mean = mean(T_sh_No_shift, na.rm = TRUE),
    lo = t.test(T_sh_No_shift)$conf.int[1],
    up = t.test(T_sh_No_shift)$conf.int[2],
    gr = "Short-lived") 

#long-lived species  
d.res_lo <- d %>% 
  group_by(land_use) %>% 
  dplyr::summarise(
    mean = mean(T_lo_No_shift, na.rm = TRUE),
    lo = t.test(T_lo_No_shift)$conf.int[1],
    up = t.test(T_lo_No_shift)$conf.int[2],
    gr = "Long-lived")

# Combine results of short- and long-lived spcies
d.res <- dplyr::bind_rows(d.res_sh, d.res_lo)
d.res <- d.res %>% mutate(
  HS = factor(land_use, levels = c("Managed\ngrasslands", "Forests", "Unmanaged\nopen areas")),
  gr = factor(gr, levels = c("Short-lived", "Long-lived"))) 

# Graphics
(p1 <- ggplot(d.res, aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = tref, col = "grey80", lwd = 0.8)  +
  geom_point(position = position_dodge(width = 0.25), cex = 1.7) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#00BFC4", "#C77CFF")) +
  ylim(-210, 210) + 
  labs(
    x = "",
    y = "Notional elevation shift (NES)\n[m per decade]",
    title = "Bryophytes"
  ) +
  theme(legend.position="bottom"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot: Notial elevation shift of short and long-lived vascular plants: differnces between land use types ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#short-lived species
d <- dat
d$land_use[d$land_use == "forest"] <- "Forests"
d$land_use[d$land_use == "grassland"] <- "Managed\ngrasslands"
d$land_use[d$land_use == "unused"] <- "Unmanaged\nopen areas"

d.res_sh <- d %>% 
  group_by(land_use) %>% 
  dplyr::summarise(
    mean = mean(T_sh_Pl_No_shift, na.rm = TRUE),
    lo = t.test(T_sh_Pl_No_shift)$conf.int[1],
    up = t.test(T_sh_Pl_No_shift)$conf.int[2],
    gr = "Short-lived") 

#long-lived species  
d.res_lo <- d %>% 
  group_by(land_use) %>% 
  dplyr::summarise(
    mean = mean(T_lo_Pl_No_shift, na.rm = TRUE),
    lo = t.test(T_lo_Pl_No_shift)$conf.int[1],
    up = t.test(T_lo_Pl_No_shift)$conf.int[2],
    gr = "Long-lived")

# Combine results of short- and long-lived spcies
d.res <- dplyr::bind_rows(d.res_sh, d.res_lo)
d.res <- d.res %>% mutate(
  HS = factor(land_use, levels = c("Managed\ngrasslands", "Forests", "Unmanaged\nopen areas")),
  gr = factor(gr, levels = c("Short-lived", "Long-lived"))) 

# Graphics
(p2 <- ggplot(d.res, aes(y = mean, x = HS, col = gr, ymin = lo, ymax = up)) +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  geom_abline(slope = 0, intercept = tref, col = "grey80", lwd = 0.8)  +
  geom_point(position = position_dodge(width = 0.25), cex = 1.7) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.25)) +
  scale_color_manual(values = c("#00BFC4", "#C77CFF")) +
  ylim(-170, 210) + 
  labs(
    x = "",
    y = "Notional elevation shift (NES)\n[m per decade]",
    title = "Vascular plants"
  ) +
  theme(legend.position="bottom"))

p1 + p2 + plot_layout(guides = "collect")
ggsave("Figures/Thermophilisation-short-long-lived_landuse.pdf", width = 10, height = 4)

