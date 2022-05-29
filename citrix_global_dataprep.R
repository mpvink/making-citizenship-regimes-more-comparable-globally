#Making Citizenship Regimes (More) Comparable Globally: Exploring the New GLOBALCIT Citizenship Law Dataset
#Maarten Vink
#CITRIX_Global and validation

#start with clean workspace
rm(list=ls(all=TRUE))

#load packages
library(reshape)
library(tidyr)
library(dplyr)
library(xlsx)
library(JWileymisc)
library(data.table)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(remotes)

#import modes_plus data
data <- read.csv("data.csv")
View(data) #190 obs of 109 variables
summary(data)

#calculate citrix based on subset of countries: only democracies and only if birth_registration > 90
# and presence of residence-based naturalisation (A06)
# and exclude BTN with A06a yrs == 20 to avoid skewed variable residuration
# N = 69
data_sel <- filter(data, dem_dum == 'Democracy' & birth_reg > 90 & A06_bin == 1 & A06a_yrs<16)
#or: data_sel <- filter(modes, dem_dum == 'Democracy' & birth_reg > 90 & A06_bin == 1 & iso3!='BTN')
View(data_sel) #68 obs

citrix_global <- data_sel[c(1:3, 106:107, 109)]
View(citrix_global) #68 obs of 6 variables

#recode jussoli2: import and recode jussoli2 from A02a and A05
#comment: coding does not take into account procedure, cf. Schmid, as not included in GLOBALCIT coding
citrix_global$jussoli2 <- ifelse(data_sel$A02a_cat == 1, 100, #unconditional ius soli
                                 ifelse(data_sel$A02a_cat == 2, 67, #conditional on parental residence
                                        ifelse( data_sel$A02a_cat != 1 & data_sel$A02a_cat != 2 &
                                                  (data_sel$A05_cat == 1 | data_sel$A05_cat == 2) , 33, 
                                                # only access after birth (A05)
                                                0))) # no ius soli or only for specific groups
count(citrix_global$jussoli2)

#import and recode jussoli3 from A02b (and A02a)
#Comment: 100 incl if conditional upon parent’s residence status (or if A02a unconditional ius soli)
#Comment: 50 based on both parents born in country
#Does not take into account procedure (automatic v application), cf. Schmid, as not included in GLOBALCIT coding
citrix_global$jussoli3 <- ifelse(data_sel$A02a_cat == 1 | data_sel$A02b_cat == 1 | data_sel$A02b_cat == 3, 100,
                                 ifelse(data_sel$A02b_cat == 2 , 50, #only if the father is also born in country
                                        0)) # no double ius soli 
count(citrix_global$jussoli3)

#import residence duration from A06a_yrs
citrix_global$residuration <- data_sel$A06a_yrs
count(citrix_global$residuration) #min = 2, max = 10 yrs
#normalisation 0 to 100 with 100 most inclusive
citrix_global$residur <- (citrix_global$residuration - max(citrix_global$residuration))/(min(citrix_global$residuration) - max(citrix_global$residuration))*100
count(citrix_global$residur)

# import dualcit from A06b
# 100 if no requirement
# comment: 50 if major exceptions apply (beyond refugees or if cannot renounce)
# comment: 0 if no substantial exceptions
citrix_global$dualcit <- ifelse(data_sel$A06b_cat == 0, 100,
                                ifelse(data_sel$A06b_cat == 1 | data_sel$A06b_cat == 2, 50,
                                       0))
count(citrix_global$dualcit)

# import langtest from A06c
# comment: required language level(cf. Schmid/MIPEX) not coded in GLOBALCIT
# comment: code 100 if No language condition indicated in the law or 
# comment: code code 50 if General assimilation condition OR Language condition without certification or test
# comment: code 0 if Language condition with certification or test

citrix_global$langtest <- ifelse(data_sel$A06c_cat == 0, 100, #No language condition indicated in the law
                                 ifelse(data_sel$A06c_cat == 1 | data_sel$A06c_cat == 2 , 50, #  General assimilation condition OR Language condition without certification or test
                                        0)) # Language condition with certification or test

count(citrix_global$langtest)

# import cittest from A06d
# comment: 100 if no condition 
# comment: 50 if condition but no test
# comment: 0 if condition and test
citrix_global$cittest <- ifelse(data_sel$A06d_cat == 0, 100,  # No condition indicated in the law 
                                ifelse(data_sel$A06d_cat == 1, 50, # condition without certification or test
                                       0)) # Condition with certification or test

count(citrix_global$cittest)


# import ecoreq from A06f
# only coded binary in GLOBALCIT, no income levels available
# comment: coded 0 if income requirement specified (justification: indicates detail of naturalisation procedure)

citrix_global$ecoreq <- ifelse(data_sel$A06f_cat == 0, 100, #No requirement relating to income, employment or welfare dependency 
                               0) #economic requirement      

count(citrix_global$ecoreq)

# import crimreq from A06e
# imprisonment years not counted in GLOBALCIT
# main distinction: specifying crimes or only general good moral character condition
# comment: NA counted as 0
citrix_global$crimreq <- ifelse(data_sel$A06e_cat == 0, 100, # No criminal record or good moral character condition
                                ifelse(data_sel$A06e_cat == 1 | data_sel$A06e_cat == 2, 67, #specification under which conditions naturalisation can be denied
                                       ifelse(data_sel$A06e_cat == 3 | data_sel$A06e_cat == 4, 33, #specified crimes + good moral character
                                              0))) #only general moral character condition applies
count(citrix_global$crimreq)

# apply Schmid aggregation formula
citrix_global$citrix_global <- round(((2*citrix_global$jussoli2 + citrix_global$jussoli3)/3 + 
                                            citrix_global$residur +
                                            citrix_global$dualcit +
                                            (citrix_global$langtest + citrix_global$cittest + citrix_global$ecoreq + citrix_global$crimreq)/4)/4, digits =0)
View(citrix_global)


#import CITRIX v2 file, incl MIPEX

#Sources:
#CITRIX v2 from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XHZV8V (file 'CITRIX v1 and v2 _ csv format')
#MIPEX data: https://www.mipex.eu/download-pdf (file 'Policy Indicators Scores (2007-2019) – core set of indicators', data from 'Citizenship Strand': MIPEX_ATN2020)
#manually combined into file 'citrix_mipex.csv')

citrix_mipex <- read.csv("citrix_mipex.csv")
View(citrix_mipex)
#subset year 2019
citrix_mipex_2019 <- subset(citrix_mipex, year==2019)
View(citrix_mipex_2019) #52 obs on 75 variables

citrix <- merge(citrix_global, citrix_mipex_2019[, c('iso3', 'CITRIX_v2', 'MIPEX_ATN_2020')],by="iso3", all=TRUE)
View(citrix) #76 obs of 19 variables

#validity checks
library("ggpubr")

# check overlapping cases 
#import citrix & CITRIX v2 data
citrix_global_citrix <- na.omit(citrix[c(1,17,18)])
View(citrix_global_citrix) #23 obs

#import citrix & MIPEX data
citrix_global_mipex <- na.omit(citrix[c(1,17,19)])
View(citrix_global_mipex) #44 obs


#correlation tests

# correlation test CITRIX_v2 and MIPEX_ATN_2020
cor.test(citrix$CITRIX_v2, citrix$MIPEX_ATN_2020, method=c("pearson", "kendall", "spearman"))
#Pearson's product-moment correlation: 0.9098138 
# 95 percent confidence interval: 0.7962133 - 0.9614471

# correlation test citrix_select and CITRIX_v2
cor.test(citrix$citrix_global_sel, citrix$CITRIX_v2, method=c("pearson", "kendall", "spearman"))
#Pearson's product-moment correlation: 0.8790461 
# 95 percent confidence interval: 0.7321258 0.9478132

# correlation test citrix_select and MIPEX_ATN_2020
cor.test(citrix$citrix_global_sel, citrix$MIPEX_ATN_2020, method=c("pearson", "kendall", "spearman"))
#Pearson's product-moment correlation: 0.8075143 
# 95 percent confidence interval:  0.6716526 0.8908311


#plot CITRIX_v2, N = 23
p1 <- ggscatter(citrix, x = "CITRIX_v2", y = "citrix_global_sel", 
                add = "reg.line", conf.int = TRUE,
                label = "iso3", repel = TRUE, font.label = c(16, "blue"),
                xlab = "CITRIX_v2", ylab = "CITRIX Global")+
  theme_minimal()+
  theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title = element_text(size = rel(2)))+
  stat_cor(label.x = 20, p.accuracy = 0.001, r.accuracy = 0.01, size = 10)
scatter_citrix <- ggpar(p1, xlim=c(0,100),
                        main = "CITRIX Global (2020) v CITRIX v2 (2019), N=23",
                        font.main = c(20))

#plot MIPEX_ATN_2020, N=45

p2 <- ggscatter(citrix, x = "MIPEX_ATN_2020", y = "citrix_global_sel", 
                add = "reg.line", conf.int = TRUE,
                label = "iso3", repel = TRUE, font.label = c(16, "blue"),
                xlab = "MIPEX 2020", ylab = "CITRIX Global")+
  theme_minimal()+
  theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title = element_text(size = rel(2)))+
  stat_cor(label.x = 20, p.accuracy = 0.001, r.accuracy = 0.01, size = 10)
scatter_mipex <- ggpar(p2, xlim=c(0,100),
                       main = "CITRIX Global (2020) v MIPEX 2020, N=44",
                       font.main = c(20)
                       
)

#combined scatter plot
jpeg('Figure8.scatter_comb.jpeg',  width = 20, height = 10, units = 'in', res = 100)
grid.arrange(arrangeGrob(scatter_citrix, scatter_mipex, nrow=1))
dev.off()

#correlation

# Select columns of interest
citrix_corr <- citrix %>% 
  select(CITRIX_v2, MIPEX_ATN_2020, citrix_global_sel)
# Inspect the data
head(citrix_corr, 3)
#correlation
library(corrr)
#with pearson (default)
res.cor <- correlate(citrix_corr)
res.cor
#term              CITRIX_v2 MIPEX_ATN_2020 citrix_global_sel
#CITRIX_v2            NA              0.910             0.879
#MIPEX_ATN_2020        0.910         NA                 0.808
#citrix_global_sel     0.879          0.808            NA    
res.cor <- correlate(citrix_corr, method = "spearman", diagonal = 1)
res.cor
#Focus on correlations of one variable with all others
#Extract the correlation with CITRIX Global
res.cor %>% 
  focus(citrix_global_sel)
#term           citrix_global_sel
#CITRIX_v2                  0.879
#MIPEX_ATN_2020             0.808

#correlation matrix
#https://github.com/r-link/corrmorant

#select three variables
corrvar <- citrix_global[17:19]

# correlation
??corrmorant
remotes::install_github("r-link/corrmorant")
library(corrmorant)
#plot
corrmorant(corrvar, style = "blue_red")
#plot
p1 <- ggcorrm(corrvar) +
  lotri(geom_point(alpha = 0.5)) +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3) +
  dia_density(lower = 0.3, fill = "grey80", color = 1)
p1

#dotplot CITRIX scores

citrix_global_na.omit <- subset(citrix, citrix_global_sel > 0)
View(citrix_global_na.omit) # 68 obs

p1 <- ggdotchart(citrix_global_na.omit, x = "iso3", y = "citrix_global_sel",
                 sorting = "descending",                       # Sort value in descending order
                 color = "citrix_global_sel",                     # colour by citrix score
                 add = "segments",                             # Add segments from y = 0 to dots
                 rotate = TRUE,                                # Rotate vertically
                 dot.size = 12,                                 # Large dot size
                 label = citrix_global_na.omit$citrix_global_sel,  # Add values as dot labels
                 font.label = list(color = "white", size = 13, 
                                   vjust = 0.5),               # Adjust label parameters
                 ylab = "CITRIX Global score",
                 xlab = "",
                 ggtheme = theme_minimal(base_size=18)  # ggplot2 theme
)
lollipop_citrix <- ggpar(p1,
                         legend = "right", legend.title = "100 = most inclusive\n
0 = most restrictive",
                         main = "Figure 9. CITRIX Global, N=68, 2020",
                         caption = "\nSource: GLOBALCIT, calculation based on Schmid (2021)")+
  theme(plot.caption = element_text(hjust = 0))+
  gradient_color(c("red",  "darkgreen"))

jpeg('Fig9.lollipop.citrix.jpeg',  width = 24, height = 24, units = 'in', res = 100)
lollipop_citrix
dev.off()



