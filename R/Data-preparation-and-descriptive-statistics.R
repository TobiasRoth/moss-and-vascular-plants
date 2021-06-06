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
library(readxl)
library(openxlsx)

# Connection to data base
db <- src_sqlite(path = "~/Dropbox/3_Resourcen/BDM_DB/DB_BDM_2020_07_01.db", create = FALSE)
db_neu <- src_sqlite(path = "~/Dropbox/3_Resourcen/BDM_DB/DB_BDM_2021_06_01.db", create = FALSE)

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
    land_use = HN,
    Delarze = as.integer(Delarze1)) %>% 
  
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
      left_join(tbl(db_neu, "TRAITS_PL"), copy = TRUE) %>% 
      as_tibble() %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_pl = n(),
        AZ_pl_Termo = sum(T >= 4, na.rm = TRUE),
        AZ_pl_Meso = sum(T == 3, na.rm = TRUE),
        AZ_pl_Cryo = sum(T <= 2, na.rm = TRUE),
        AZ_pl_sh = sum(T[!is.na(match(KS, c("rrr", "rrs", "crr")))], na.rm = TRUE),
        AZ_pl_lo = sum(T[!is.na(match(KS, c("ccc", "ccr", "ccs")))], na.rm = TRUE),
        T_pl = round(mean(T, na.rm = TRUE), 2),
        T_pl_sh = mean(T[!is.na(match(KS, c("rrr", "rrs", "crr")))], na.rm = TRUE),
        T_pl_lo = mean(T[!is.na(match(KS, c("ccc", "ccr", "ccs")))], na.rm = TRUE)
      ),
    copy = TRUE) %>% 
  
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
        T_mo = round(mean(T, na.rm = TRUE), 2),
        T_mo_sh = mean(T[GenTime <= 6.6 & !is.na(GenTime)], na.rm = TRUE),
        T_mo_lo = mean(T[GenTime > 6.6 & !is.na(GenTime)], na.rm = TRUE)
      )) %>% 
  as_tibble() %>% 
  replace_na(list(AZ_pl = 0, AZ_pl_Termo = 0, AZ_pl_Meso = 0, AZ_pl_Cryo = 0, AZ_pl_sh = 0, AZ_pl_lo = 0,
                  AZ_mo = 0, AZ_mo_Termo = 0, AZ_mo_Meso = 0, AZ_mo_Cryo = 0, AZ_mo_sh = 0, AZ_mo_lo = 0)) 

# Add name of delarze habitat type
surveys <- surveys %>% 
  left_join(
    read_excel("Data-raw/Teil_II-UV-1709-NPA_NPL-DFI_DigitaleListe_Lebensraeume.xlsx", skip = 9) %>% 
      transmute(
        Ecosystem = `Ecosystem (Deusch)`,
        Delarze = str_remove_all(Typo_CH, pattern = "[.]") %>% str_sub(1, 3) %>% as.integer(),
        Delarze_Name = Deutsch) %>% 
      group_by(Ecosystem, Delarze) %>% 
      dplyr::summarise(Delarze_Name = first(Delarze_Name))
  )

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
surveys$HS <- factor(surveys$HS, levels = c("colline", "montane", "subalpine", "alpine"))

# Remove 1 outlier (Plot with 1 cryophilous bryophyte species at 358 asl, in the
# floodplain of the Maggia river, the moss presumbably has been floated from
# further above)
surveys <- surveys[surveys$aID_KD!=3090399990,]

# Remove special habitat types from unused land use type (gravel pits, Adlerfarnflur, Hochmoor)
exclude <- c("3180685497", "3337759823", "380195001", "97131301", "3136263783", "3293704708", 
             "3368207749", "486553630", "3232254918", "360674069", "518802970",
             "609876239", "3108478857", "613235264", "3117032614")
surveys <- surveys %>% 
  dplyr::filter(is.na(match(aID_KD, exclude))) 

# Correct land-use types
surveys$land_use[surveys$aID_STAO == 659182] <- "forest"
surveys$land_use[surveys$aID_STAO == 557138] <- "grassland"
surveys$land_use[surveys$aID_STAO == 569206] <- "grassland"
surveys$land_use[surveys$aID_STAO == 695222] <- "grassland"
surveys$land_use[surveys$aID_STAO == 593130] <- "unused"
surveys$land_use[surveys$aID_STAO == 605114] <- "unused"
surveys$land_use[surveys$aID_STAO == 665154] <- "grassland"
surveys$land_use[surveys$aID_STAO == 695170] <- "grassland"
surveys$land_use[surveys$aID_STAO == 749150] <- "grassland"
surveys$land_use[surveys$aID_STAO == 785198] <- "unused"
surveys$land_use[surveys$aID_STAO == 827182] <- "unused"

# Remove plot with no species recorded
surveys <- surveys %>% 
  filter(aID_STAO != "797174")

# Select sites with at least 3 surveys and no landuse change
ausw <- surveys %>% 
  group_by(aID_STAO) %>% 
  dplyr::summarise(
    n = n(),
    n_lut = n_distinct(land_use),
    n_Pl = sum(!is.na(T_pl)),
    n_Mo = sum(!is.na(T_mo))
  ) %>% 
  filter(n >= 3 & n_lut == 1)
surveys <- surveys %>% 
  filter(!is.na(match(aID_STAO, ausw$aID_STAO)))

# Number of plots
n_distinct(surveys$aID_STAO)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export vascular plant and bryophyte data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bryophytes
moss <- 
  tbl(db, "Moos") %>% 
  left_join(tbl(db, "KD_Z9")) %>% 
  left_join(tbl(db, "ARTEN") %>% dplyr::select(aID_SP, Gattung, Art, Komplex)) %>%      
  left_join(tbl(db, "Traits_Moos")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, surveys$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_STAO = aID_STAO,
    aID_SP = aID_SP,
    species = paste(Gattung, Art),
    T = T,
    GenTime = GenTime,
    Komplex = Komplex
  ) %>% 
  arrange(aID_KD, aID_SP)

# Vascular plants
plants <- 
  tbl(db, "PL") %>% 
  left_join(tbl(db, "KD_Z9")) %>% 
  left_join(tbl(db, "ARTEN") %>% dplyr::select(aID_SP, Gattung, Art, Komplex)) %>%      
  left_join(tbl(db_neu, "Traits_Pl"), copy = TRUE) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, surveys$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_STAO = aID_STAO,
    aID_SP = aID_SP,
    species = paste(Gattung, Art),
    T = T,
    KS = KS,
    Komplex = Komplex
  ) %>% 
  arrange(aID_KD, aID_SP)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare site data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get temporal trend
get_trend <- function(mes, year) {
  res <- NA
  try({
    dd <- tibble(y = mes[!is.na(mes)], yr = (year[!is.na(mes)] -2010)/10) 
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
    HS = first(HS),
    land_use = first(land_use),
    N_surveys = n(),
    SR_pl_mean = mean(AZ_pl, na.rm = TRUE),
    SR_pl_sh_mean = mean(AZ_pl_sh, na.rm = TRUE),
    SR_pl_lo_mean = mean(AZ_pl_lo, na.rm = TRUE),
    SR_pl_trend = get_trend(AZ_pl, year),
    T_pl_mean = mean(T_pl, na.rm = TRUE),
    T_pl_sh_mean = mean(T_pl_sh, na.rm = TRUE),
    T_pl_lo_mean = mean(T_pl_lo, na.rm = TRUE),    
    T_pl_trend = get_trend(T_pl, year),
    T_pl_sh_trend = get_trend(T_pl_sh, year),
    T_pl_lo_trend = get_trend(T_pl_lo, year),
    SR_mo_mean = mean(AZ_mo, na.rm = TRUE),
    SR_mo_sh_mean = mean(AZ_mo_sh, na.rm = TRUE),
    SR_mo_lo_mean = mean(AZ_mo_lo, na.rm = TRUE),
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
# Prepare species list ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bryophytes
moss %>% 
  dplyr::mutate(
    Species = species,
    `Species group` = as.integer(Komplex != "nein"),
    `Temperature value` = T,
    Short_lived = as.integer(GenTime <= 6.6 & !is.na(GenTime)),
    Long_lived = as.integer(GenTime > 6.6 & !is.na(GenTime))
  ) %>% 
  group_by(Species, `Species group`, `Temperature value`, Short_lived, Long_lived) %>% 
  dplyr::summarise(
    N_records = n()
  ) %>% 
  openxlsx::write.xlsx(file = "Data-raw/Bryophytes_species_list.xlsx")

# Vascular plants
plants %>% 
  dplyr::mutate(
    Species = species,
    `Species group` = as.integer(Komplex != "nein" & !is.na(Komplex)),
    `Temperature value` = T,
    Short_lived = as.integer(!is.na(match(KS, c("rrr", "rrs", "crr")))),
    Long_lived = as.integer(!is.na(match(KS, c("ccc", "ccr", "ccs"))))
  ) %>% 
  group_by(Species, `Species group`, `Temperature value`, Short_lived, Long_lived) %>% 
  dplyr::summarise(
    N_records = n()
  ) %>% 
  openxlsx::write.xlsx(file = "Data-raw/Vascular_plant_species_list.xlsx")

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
dat %>% nrow

# Number of plots in the nival zone
sum(dat$elevation >= 2800)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Descriptive statistics (Results) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Number of surveys 
mean(dat$N_surveys) %>% round(1)
sd(dat$N_surveys) %>% round(1)
nrow(surveys)

# Total number of recorded species
n_distinct(moss$aID_SP)
n_distinct(plants$aID_SP)

# Proportion of aggregates
tt <- tbl(db, "Arten") %>% 
  filter(Z7Z9 == 1) %>% 
  filter(MOOS == 1) %>% 
  pull(Komplex)
mean(tt != "nein")
mean(moss$Komplex != "nein" )
tt <- tbl(db, "Arten") %>% 
  filter(Z7Z9 == 1) %>% 
  filter(PL == 1) %>% 
  pull(Komplex)
mean(!is.na(tt))
mean(!is.na(plants$Komplex == "nein"))

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

# Elevational range
range(dat$elevation)

# Proportion of mosses with trait values
tmp <- moss %>% 
  group_by(species) %>% 
  dplyr::summarise(T = mean(T)) 
mean(!is.na(tmp$T))
mean(!is.na(moss$T))

# Proportion of vascular plants with trait values
tmp <- plants %>% 
  group_by(species) %>% 
  dplyr::summarise(T = mean(T)) 
mean(!is.na(tmp$T))
mean(!is.na(plants$T))

# Proportion of sites with no estimates for thermophilsation
mean(is.na(dat$T_mo_trend))
mean(is.na(dat$T_pl_trend))

# Number of sites with estimates of thermophilsation
sum(!is.na(dat$T_mo_trend))
sum(!is.na(dat$T_pl_trend))

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
# owncol <- rep("#66C2A5", nrow(dat))
# owncol[dat$land_use == "grassland"] <- "#A6D854"
# owncol[dat$land_use == "unused"] <- "#B3B3B3"
owncol <- rep("green", nrow(dat))
owncol[dat$land_use == "grassland"] <- "orange"
owncol[dat$land_use == "unused"] <- "white"

pdf("Figures/Map-with-study-plots.pdf", width = 7, height = 4.1)
par(mar = c(0,0,2,0))
plot(NA, xlim = c(490000, 840000), ylim = c(60000, 300000), type = "n", axes = FALSE, asp = 1)
plot(ch, add =TRUE)
plot(gadm, add = TRUE)
plot(seen[1:13,], add = TRUE, col = seecol, border = seecol, lwd = 0.01)
points(coordID2coord(dat$aID_STAO), pch = 16, cex = 0.6)
points(coordID2coord(dat$aID_STAO), pch = 16, cex = 0.5, col = owncol)
xx <- 490000
yy <- 80000
lines(x=c(xx, xx+50000), y=c(yy, yy))
lines(x=c(xx, xx), y=c(yy, yy+2000))
lines(x=c(xx+50000, xx+50000), y=c(yy, yy+2000))
text(xx+25000, yy-5000, "50 km", cex=0.7)
legend(
  740000, yy + 34000, bty = "n",
  legend = c("Forests", "Managed grasslands", "Unmanaged open areas"), 
  pch = 21,
  pt.bg = c("green", "orange", "white"))
dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save data for further analyses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# New aID_STAO
surveys$aID_STAO <- match(surveys$aID_STAO, dat$aID_STAO)
surveys <- surveys[, -1]
dat$aID_STAO <- 1:nrow(dat)

# Save data
write_csv(surveys, file = "Data-raw/surveys.csv")
write_csv(dat, file = "Data-raw/dat.csv")

