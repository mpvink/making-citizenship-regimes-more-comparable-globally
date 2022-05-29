#Making Citizenship Regimes (More) Comparable Globally: Exploring the New GLOBALCIT Citizenship Law Dataset
#Maarten Vink
#Data preparation

#start with clean workspace
rm(list=ls(all=TRUE))

#load packages
library(reshape)
library(tidyr)
library(dplyr)
library(xlsx)

#Import data
# download data from: https://hdl.handle.net/1814/73190  
# file 'data_v1.0_country-year.csv'
data <- read.csv("data_v1.0_country-year.csv")

#View data
View(data) # 190 obs of 104 variables

# import vdem
load(url("https://github.com/vdeminstitute/vdemdata/raw/master/data/vdem.RData"))
vdem_2020 <- filter(vdem, year == 2020) %>% 
  select(country_name, year, country_text_id, v2x_regime)
View(vdem_2020)
#rename 
names(vdem_2020)[3] <- "iso3"
View(vdem_2020)

#remove missing modes rows
#missing from modes: HKG, MDV, PSE, PSG, SML, THA, TLS, ZZB
data_dem <- subset(vdem_2020, iso3 != 'HKG' & iso3!='MDV' & iso3 !='PSE' & 
                     iso3 != 'PSG' & iso3 !='SML' &
                     iso3 !='THA' & iso3 != 'ZZB')
View(data_dem) # 172 obs of 2 variables

# add new dummy variable 'v2x_regime_dem'
data_dem$dem_dum <- NA
data_dem$dem_dum <- data_dem$v2x_regime
# add new categorical variable 'dem_cat'
data_dem$dem_cat <- NA
data_dem$dem_cat <- data_dem$v2x_regime
#recode Democracy (dummy)  
data_dem$dem_dum[data_dem$dem_dum==0] <- "Autocracy"
data_dem$dem_dum[data_dem$dem_dum==1] <- "Autocracy"
data_dem$dem_dum[data_dem$dem_dum==2] <- "Democracy"
data_dem$dem_dum[data_dem$dem_dum==3] <- "Democracy"

#recode Democracy (categorical)
data_dem$dem_cat[data_dem$dem_cat==0] <- "Closed Autocracy"
data_dem$dem_cat[data_dem$dem_cat==1] <- "Electoral Autocracy"
data_dem$dem_cat[data_dem$dem_cat==2] <- "Electoral Democracy"
data_dem$dem_cat[data_dem$dem_cat==3] <- "Liberal Democracy"

View(data_dem) #172 obs of 4 variables

# Merge vdem data and modes
dat <- merge(data, data_dem,by=c("iso3", "year"), all = TRUE)
View(dat) # 190 observations of 108 variables
summary(dat)

#VDEM add missing countries
# missing: NA's == 18

#missing VDEM data
#missing 
#manual recode based on information from FREEDOM HOUSE
#alternatively, use this package: https://xmarquez.github.io/democracyData/index.html

#https://freedomhouse.org/countries/freedom-world/scores
#FREEDOM STATUS: Free or Partly Free, score >55  -> Democracy
#FREEDOM STATUS: Not Free -> Autocracy

#ATG  AG  AntiguaBarbuda, 
#https://freedomhouse.org/country/antigua-and-barbuda/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 85
#ElecDem
dat$dem_dum[dat$iso3=="ATG"] <- 'Democracy'

#BHS  BS  Bahamas
#https://freedomhouse.org/country/bahamas/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 91
#LibDem
dat$dem_dum[dat$iso3=="BHS"] <- 'Democracy'

#BLZ  BZ  Belize
#https://freedomhouse.org/country/belize/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 86
#LibDem
dat$dem_dum[dat$iso3=="BLZ"] <- 'Democracy'

#BRN  BN  Brunei
#https://freedomhouse.org/country/brunei/freedom-world/2020
#FREEDOM STATUS: Not Free, AGGREGATE SCORE: 28
#ClosedAut
dat$dem_dum[dat$iso3=="BRN"] <- 'Autocracy'

#DMA  DM	Dominica
#https://freedomhouse.org/country/dominica/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 93
#LibDem
dat$dem_dum[dat$iso3=="DMA"] <- 'Democracy'

#FSM Micronesia
#https://freedomhouse.org/country/micronesia/freedom-world/2020
#Total Score and Status: 92 Free
#LibDem
dat$dem_dum[dat$iso3=="FSM"] <- 'Democracy'

#GRD  GD	Grenada
#https://freedomhouse.org/country/grenada/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 89
#LibDem
dat$dem_dum[dat$iso3=="GRD"] <- 'Democracy'

#KIR Kiribati
#https://freedomhouse.org/country/kiribati/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 93
#LibDem
dat$dem_dum[dat$iso3=="KIR"] <- 'Democracy'

#KNA  KN	SaintKittsNevis
#https://freedomhouse.org/country/st-kitts-and-nevis/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 89
#LibDem
dat$dem_dum[dat$iso3=="KNA"] <- 'Democracy'

#LCA  LC	SaintLucia
#https://freedomhouse.org/country/st-lucia/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 92
#LibDem
dat$dem_dum[dat$iso3=="LCA"] <- 'Democracy'

#LIE  LI	Liechtenstein
#https://freedomhouse.org/country/liechtenstein/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 90
#LibDem
dat$dem_dum[dat$iso3=="LIE"] <- 'Democracy'

#LKA  LK	SriLanka
#https://freedomhouse.org/country/sri-lanka/freedom-world/2020
#FREEDOM STATUS: Partly Free, AGGREGATE SCORE: 56
#ElecAut
dat$dem_dum[dat$iso3=="LKA"] <- 'Democracy'

#MHL Marshall Islands
#https://freedomhouse.org/country/marshall-islands/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 93
#LibDem
dat$dem_dum[dat$iso3=="MHL"] <- 'Democracy'

#NRU Nauru
#https://freedomhouse.org/country/nauru/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 77
#LibDem
dat$dem_dum[dat$iso3=="NRU"] <- 'Democracy'

#PLW Palau
#https://freedomhouse.org/country/palau/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 92
#LibDem
dat$dem_dum[dat$iso3=="PLW"] <- 'Democracy'

#TON Tonga
#https://freedomhouse.org/country/tonga/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 79
#ElecDem
dat$dem_dum[dat$iso3=="TON"] <- 'Democracy'

#TUV Tuvalu
#https://freedomhouse.org/country/tuvalu/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 93
#LibDem
dat$dem_dum[dat$iso3=="TUV"] <- 'Democracy'

#WSM Western Samoa
#https://freedomhouse.org/country/samoa/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 81
#ElecDem
dat$dem_dum[dat$iso3=="WSM"] <- 'Democracy'

#VCT  VC	SaintVincentGrenadines
#https://freedomhouse.org/country/st-vincent-and-grenadines/freedom-world/2020
#FREEDOM STATUS: Free, AGGREGATE SCORE: 91
#ElecDem
dat$dem_dum[dat$iso3=="VCT"] <- 'Democracy'

View(dat) #190 obs of 108 variables
summary(dat)

# Add birth registration data
# data from https://data.unicef.org/topic/child-protection/birth-registration/#
birth_reg <- read.csv("UNHCR_2021birth_registration.csv") [, c("iso3", "birth_reg")]
View(birth_reg) #189 observations of 2 variables
summary(birth_reg)

# Merge birth registration and modes_plus
dat_plus <- merge(dat, birth_reg, by="iso3", all=TRUE)
View(dat_plus) # 191 observations of 108 variables

#remove Thailand
dat_plus <- dat_plus[dat_plus$iso3 != "THA",]
View(dat_plus) # 190 observations of 109 variables
# Export complete modes data with vdem and birth registration data
write.table(dat_plus, file = "data.csv", sep = ",", quote = FALSE, row.names = F)


