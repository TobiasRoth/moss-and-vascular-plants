rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(BDM)
library(simba)
library(raster)

# Connection to data base
db <- src_sqlite(path = "DB/DB_BDM_2020_07_01.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export survey data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# One line in "surveys" contains one survey (each plot is surveyed once every five
# years). Surveys are included only if vascular plant and bryophyte surveys were
# considered valid with sufficient data quality.

surveys <- 
  
  # Select surveys
  tbl(db, "KD_Z9") %>% 
  filter(HN != "Aecker" & HN != "Gletscher, Wasser" & HN != "Siedlung") %>% 
  filter(!is.na(yearMoos) & !is.na(yearPl)) %>% 
  filter(Aufnahmetyp == "Normalaufnahme_Z9" | Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9") %>% 
  transmute(
    aID_KD = aID_KD, 
    aID_STAO = aID_STAO, 
    year = yearP,
    yr = (yearP - 2010) / 10,
    land_use = HN) %>% 
  
  # Add site data:
  left_join(
    tbl(db, "Raumdaten_Z9") %>% 
      transmute(
        aID_STAO = aID_STAO, 
        elevation = Hoehe,
        ele = (Hoehe - 500) / 200,
        HS = HS
      )) %>% 
  
  # Add vascular plant data:
  left_join(
    tbl(db, "Pl") %>% 
      left_join(tbl(db, "TRAITS_PL")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_pl = n(),
        AZ_pl_Termo = sum(T >= 4, na.rm = TRUE),
        AZ_pl_Meso = sum(T == 3, na.rm = TRUE),
        AZ_pl_Cryo = sum(T <= 2, na.rm = TRUE),
        T_pl = mean(T, na.rm = TRUE) %>% round(2)
      )) %>% 
  
  # Add bryophyte data:
  left_join(
    tbl(db, "Moos") %>% 
      left_join(tbl(db, "Traits_Moos")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_mo = n(),
        AZ_mo_Termo = sum(T >= 4, na.rm = TRUE),
        AZ_mo_Meso = sum(T == 3, na.rm = TRUE),
        AZ_mo_Cryo = sum(T <= 2, na.rm = TRUE),
        AZ_mo_sh = sum(T[GenTime <= 6.6 & !is.na(GenTime)], na.rm = TRUE),
        AZ_mo_lo = sum(T[GenTime > 6.6 & !is.na(GenTime)], na.rm = TRUE),
        T_mo = mean(T, na.rm = TRUE) %>% round(2),
        T_mo_sh = mean(T[GenTime <= 6.6 & !is.na(GenTime)], na.rm = TRUE),
        T_mo_lo = mean(T[GenTime > 6.6 & !is.na(GenTime)], na.rm = TRUE)
      )) %>% 
  as_tibble() %>% 
  replace_na(list(AZ_pl = 0, AZ_pl_Termo = 0, AZ_pl_Meso = 0, AZ_pl_Cryo = 0,
                  AZ_mo = 0, AZ_mo_Termo = 0, AZ_mo_Meso = 0, AZ_mo_Cryo = 0, AZ_mo_sh = 0, AZ_mo_lo = 0)) 

# Rename land-use types
surveys$land_use[surveys$land_use == "Nicht genutzte Flaechen"] <- "unused"
surveys$land_use[surveys$land_use == "Alpweiden"] <- "grassland"
surveys$land_use[surveys$land_use == "Wiesen, Weiden"] <- "grassland"
surveys$land_use[surveys$land_use == "Wald"] <- "forest"

# Rename HS types
surveys$HS[surveys$HS == "kollin"] <- "colline"
surveys$HS[surveys$HS == "montan"] <- "montane"
surveys$HS[surveys$HS == "subalpin"] <- "subalpine"
surveys$HS[surveys$HS == "alpin"] <- "alpine"

# Remove plots with land_use = unused in the colline, montane and subalpine zone
# These are special cases (e.g. gravel pits, waste lands) and results can hardly
# be interpreted.
surveys <- surveys %>% 
  filter(!(land_use == "unused" & HS == "colline")) %>% 
  filter(!(land_use == "unused" & HS == "montane"))

# Remove 1 outlier (Plot with 1 cryophilous bryophyte species at 358 asl, in the
# floodplain of the Maggia river, the moss presumbably has been floated from
# further above)
surveys <- surveys[surveys$aID_KD!=3090399990,]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export vascular plant and bryophyte data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bryophytes
moss <- 
  tbl(db, "Moos") %>% 
  left_join(tbl(db, "KD_Z9")) %>% 
  left_join(tbl(db, "ARTEN") %>% dplyr::select(aID_SP, Gattung, Art)) %>%      
  left_join(tbl(db, "Traits_Moos")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, surveys$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_STAO = aID_STAO,
    aID_SP = aID_SP,
    species = paste(Gattung, Art)
  ) %>% 
  arrange(aID_KD, aID_SP)

# Vascular plants
plants <- 
  tbl(db, "PL") %>% 
  left_join(tbl(db, "KD_Z9")) %>% 
  left_join(tbl(db, "ARTEN") %>% dplyr::select(aID_SP, Gattung, Art)) %>%      
  left_join(tbl(db, "Traits_Pl")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, surveys$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_STAO = aID_STAO,
    aID_SP = aID_SP,
    species = paste(Gattung, Art)
  ) %>% 
  arrange(aID_KD, aID_SP)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare site data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get temporal trend
get_trend <- function(mes, year) {
  res <- NA
  try({
    dd <- tibble(y = mes, yr = (year -2010)/10) 
    mod <- lm(y ~ yr, data = dd)
    res <- coef(mod)[2]
  })
  res
}

# Calculate averages and temporal trends per site
dat <- surveys %>% 
  group_by(aID_STAO) %>% 
  dplyr::summarise(
    elevation = mean(elevation),
    ele = (mean(elevation) - 500) / 200,
    HS = first(HS),
    land_use = first(land_use),
    N_surveys = n(),
    SR_pl_mean = mean(AZ_pl, na.rm = TRUE),
    SR_pl_trend = get_trend(AZ_pl, year),
    T_pl_mean = mean(T_pl, na.rm = TRUE),
    T_pl_trend = get_trend(T_pl, year),
    SR_mo_mean = mean(AZ_mo, na.rm = TRUE),
    SR_mo_trend = get_trend(AZ_mo, year),
    T_mo_mean = mean(T_mo, na.rm = TRUE),
    T_mo_sh_mean = mean(T_mo_sh, na.rm = TRUE),
    T_mo_lo_mean = mean(T_mo_lo, na.rm = TRUE),
    T_mo_trend = get_trend(T_mo, year),
    T_mo_sh_trend = get_trend(T_mo_sh, year),
    T_mo_lo_trend = get_trend(T_mo_lo, year)
  ) 

# Turnover between two surveys
getturnover <- function(x, specdat) {
  res <- NA
  tt <- specdat %>%
    filter(aID_STAO == dat$aID_STAO[x])
  if(n_distinct(tt$aID_KD) > 1 & n_distinct(tt$aID_SP) > 1) {
    res <-
      tt %>%
      transmute(aID_KD = aID_KD, aID_SP = aID_SP, Occ = 1) %>%
      simba::sim(method = "cocogaston", listin = TRUE, listout = TRUE) %>%
      pull(cocogaston) %>% mean
  }
  res
}

# Apply functions for all sites, independently for bryophytes and vascular
# plants
# dat$TU_mo <-  map_dbl(1:nrow(dat), getturnover, specdat = moss)
# dat$TU_pl <-  map_dbl(1:nrow(dat), getturnover, specdat = plants)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Summary statistics (Methods) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Number of plots on the systematic grid (without dangerous and lake sites)
tbl(db, "STICHPROBE_Z9") %>% 
  left_join(tbl(db, "Raumdaten_Z9")) %>% 
  dplyr::filter( BDM_Grid == "ja") %>% 
  dplyr::filter( BDM_aktuell == "ja") %>% 
  dplyr::filter( Bearbeitbarkeit == 1) %>% 
  as_tibble() %>% 
  nrow()

# Number of studied plots
nrow(dat)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Describtive statistics (Results) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Number of surveys 
mean(dat$N_surveys) %>% round(1)
sd(dat$N_surveys) %>% round(1)
mean(dat$N_surveys < 3) %>% round(3)
nrow(surveys)

# Total number of recorded species
 n_distinct(moss$aID_SP)
n_distinct(plants$aID_SP)

# Plot averages: species richness
mean(dat$SR_mo_mean) %>% round(1) 
sd(dat$SR_mo_mean) %>% round(1) 
mean(dat$SR_pl_mean) %>% round(1) 
sd(dat$SR_pl_mean) %>% round(1) 

# Plot averages: temperature values
mean(dat$T_mo_mean, na.rm = TRUE) %>% round(2) 
sd(dat$T_mo_mean, na.rm = TRUE) %>% round(2) 
mean(dat$T_pl_mean, na.rm = TRUE) %>% round(2) 
sd(dat$T_pl_mean, na.rm = TRUE) %>% round(2) 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Tabel with descriptive statistics ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat %>% 
  mutate(
    ele = (elevation - 1000) / 200,
    HS = factor(HS, levels = c("colline", "montane", "subalpine", "alpine")),
    land_use = factor(land_use, levels = c("forest", "grassland", "unused"))
  ) %>% 
  filter(!(land_use == "forest" & HS == "alpine")) %>% 
  filter(!(land_use == "unused" & HS == "subalpine")) %>% 
  group_by(`Land use type` = land_use, `Elevational zone` = HS) %>%
  dplyr::summarise(
    `Number of plots` = n(),
    `Elevation (m)` = mean(elevation),
    `Species richness` = mean(SR_mo_mean),
    `Temperature value` = mean(T_mo_mean, na.rm = TRUE)) %>% 
  rbind(
    dat %>% 
      mutate(
        ele = (elevation - 1000) / 200,
        HS = factor(HS, levels = c("colline", "montane", "subalpine", "alpine")),
        land_use = factor(land_use, levels = c("forest", "grassland", "unused"))
      ) %>% 
      filter(!(land_use == "forest" & HS == "alpine")) %>% 
      filter(!(land_use == "unused" & HS == "subalpine")) %>% 
      group_by(`Land use type` = land_use, `Elevational zone` = HS) %>%
      dplyr::summarise(
        `Number of plots` = n(),
        `Elevation (m)` = mean(elevation),
        `Species richness` = mean(SR_pl_mean),
        `Temperature value` = mean(T_pl_mean, na.rm = TRUE))) %>% 
  kable(
    digits = c(0, 0, 0, 0, 1, 2),
    align = c("l", "l", rep("r", 4)),
    booktabs = T) %>% 
  kable_styling() %>% 
  group_rows("(a) Bryophytes", 1, 8, bold = F, italic = T) %>% 
  group_rows("(b) Vascular plants", 9, 16, bold = F, italic = T)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make map with location of study plots ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load spatial data for map
load("Data-geo/ch.RData")
load("Data-geo/gadm.RData")
load("Data-geo/seen.RData")

# Colorsetting  
seecol <- brewer.pal(8, name = "Paired")[1]
sitecol <- brewer.pal(8, name = "Paired")[4]
sitecolEPT <- brewer.pal(8, name = "Paired")[8]
owncol <- rep("#66C2A5", nrow(dat))
owncol[dat$land_use == "grassland"] <- "#A6D854"
owncol[dat$land_use == "unused"] <- "#B3B3B3"

pdf("Figures/Map-with-study-plots.pdf", width = 6, height = 4)
par(mar = c(0,0,2,0))
plot(NA, xlim = c(490000, 840000), ylim = c(60000, 300000), type = "n", axes = FALSE, asp = 1)
plot(ch, add =TRUE)
plot(gadm, add = TRUE)
plot(seen[1:13,], add = TRUE, col = seecol, border = seecol, lwd = 0.01)
points(coordID2coord(dat$aID_STAO), pch = 16, cex = 0.7)
points(coordID2coord(dat$aID_STAO), pch = 16, cex = 0.5, col = owncol)
xx <- 490000
yy <- 80000
lines(x=c(xx, xx+50000), y=c(yy, yy))
lines(x=c(xx, xx), y=c(yy, yy+2000))
lines(x=c(xx+50000, xx+50000), y=c(yy, yy+2000))
text(xx+25000, yy-5000, "50 km", cex=0.7)
dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save data for further analyses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# New aID_STAO
surveys$aID_STAO <- match(surveys$aID_STAO, dat$aID_STAO)
surveys <- surveys[, -1]
dat$aID_STAO <- 1:nrow(dat)

# Save data
write_csv(surveys, path = "Data-raw/surveys.csv")
write_csv(dat, path = "Data-raw/dat.csv")

