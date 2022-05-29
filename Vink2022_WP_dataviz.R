#Making Citizenship Regimes (More) Comparable Globally: Exploring the New GLOBALCIT Citizenship Law Dataset
#Maarten Vink
#Data visualisation

#start with clean workspace
rm(list=ls(all=TRUE))

#load packages
library(reshape)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(plyr)
library(dplyr)
library(xlsx)
library(grid)
library(gridExtra)
library(JWileymisc)
library(data.table)
library(naniar)
library(rworldmap)
library(ggstatsplot)


#Import data
data1 <- read.csv("data.csv") 
#check if data.frame
class(data1) #data.frame
#view data
View(data1) #190 observations of 109 variables

#recode values 99 as NA in dataframe for A06 variables
data <- data1 %>%
  mutate_at(vars(A06a_bin:A06f_cat), na_if, 99)

View(data) #190 observations of 109 variables
summary(data)
head(data)


#Fig 1: explore 'big picture' acquisitiomn modes, based on binary variables
#Import acq_bin data incl iso3, region and dem
dat_acq_bin <- data %>%
  select(1,3,5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 26, 28, 30, 32,
         34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72,107)
View(dat_acq_bin) #190 obs on 39 variables
summary(dat_acq_bin)
class(dat_acq_bin$A01a_bin) #integer

#count number of acq modes per country
#exclude A06a-A06f
dat_acq_bin_select <- dat_acq_bin[, c(1:11, 18:37)]
View(dat_acq_bin_select) #190 obs on 31 variables
#count number of acq modes per country, count columns with acq modes
modes_acq_count <- apply(dat_acq_bin_select, 1, function(x) length(which(x[3:30]=="1")))
modes_acq_count
View(modes_acq_count)
class(modes_acq_count) # integer
#convert list into dataframe
modes_acq_count1 <- data.frame(matrix(unlist(modes_acq_count), nrow=190, byrow=TRUE),stringsAsFactors=FALSE)
View(modes_acq_count1)
#rename variable
names(modes_acq_count1)[1] <- "n.acq.modes"
View(modes_acq_count1)
#add rownames
rownames(modes_acq_count1, do.NULL = TRUE, prefix = "row")
rownames(modes_acq_count1) <- dat_acq_bin$iso3
#rename variable
modes_acq_count2 <- cbind(Row.Names = rownames(modes_acq_count1), modes_acq_count1)
#remove row.names
rownames(modes_acq_count2) <- NULL
#rename into iso3
names(modes_acq_count2)[1] <- "iso3"
View(modes_acq_count2) #190 obs
#add democracy
modes_acq_count2 <- cbind(modes_acq_count2, dat_acq_bin_select$dem_dum)
#rename 
names(modes_acq_count2)[3] <- "dem_dum"
View(modes_acq_count2) #190 obs

#count acq modes
modes_acq_count2 %>% slice_max(n.acq.modes, n = 3)
#GRC          24
#AUT          20
#CZE          20
#DEU          20
#FIN          20
modes_acq_count2 %>% slice_min(n.acq.modes, n = 3)
#URY           2
#MMR           4
#PLW           4

summary(modes_acq_count2$n.acq.modes)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00    9.00   11.00   11.53   14.00   24.00 

# Plot distribution number of acq modes
#make plot with caption
plot_acq_modes_count_capt <- ggplot(modes_acq_count2,aes(x=factor(n.acq.modes),y=stat(count), fill=factor(n.acq.modes)))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), position=position_stack(0.5), size = 4)+
  theme(legend.position='none')+
  theme_minimal(base_size = 18)+
  theme(strip.text.x = element_text(size = 11, face = "bold", hjust = 0, vjust = -1))+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))+
  labs(title = "",
       subtitle = "Mean=11.5 | Median=11 | N = 190 | 28 modes",
       caption = '')+
  xlab("Number of modes")+
  ylab("Number of countries")+
  annotate("label", x = 1, y = 22, size = 4, label = "Top 5 most frequent modes of acquisition:\n
A01b. Descent (birth abroad) (190)\n
A06. Residence-based naturalisation (175)\n
A08. Marriage-based naturalisation (168)\n
A01a. Descent (birth in country) (158)\n
A16. Reacquisition (157)",
           hjust=0)+
  annotate("label", x = 15, y = 19, size = 4, label = "Countries with least modes of acquisition:\n
Uruguay (2)\n
Myanmar (4)\n
Palau (4)\n
\n
Countries with most modes of acquisition:\n
Greece (24)\n
Austria (20)\n
Czech Republic (20)\n
Finland (20)\n
Germany (20)", hjust=0)

#save
jpeg('Fig1.acq.modes_nt.jpeg',  width = 15, height = 10, units = 'in', res = 200)
plot_acq_modes_count_capt
dev.off()

#summary stats by regime
modes_acq_count2 %>%
  group_by(dem_dum) %>%
  summarise_at(vars(n.acq.modes), tibble::lst(mean, median, sd, min, max), na.rm=TRUE)
#dem_dum    mean    median    sd   min   max
#Autocracy  10.6   10    2.9     4    19
#Democracy  12.2   11.5  4.1     2    24
  

#count number of acq modes per country
#change into long format & remove _bin
modes_acq_count <- dat_acq_bin %>% pivot_longer(names_to = "mode", values_to = "category", A01a_bin:A26_bin)
modes_acq_count$mode = sub("\\_bin.*","", as.character(modes_acq_count$mode))
View(modes_acq_count) #6460 obs of 5 variables

#recode values 0 and 1 in whole dataframe
class(modes_acq_count$category)
modes_acq_count$category <- as.character(modes_acq_count$category)
modes_acq_count$category[modes_acq_count$category=='0'] <- 'No'
modes_acq_count$category[modes_acq_count$category=='1'] <- 'Yes'
View(modes_acq_count) #6460 obs on 5 variables


#summary stats by regime
modes_acq_count2 %>%
  group_by(dem_dum) %>%
  summarise_at(vars(n.acq.modes), tibble::lst(mean, median, sd, min, max), na.rm=TRUE)
#dem_dum    mean median    sd   min   max
#Autocracy  10.6   10    2.90     4    19
#Democracy  12.2   11.5  4.11     2    24



#Import loss_bin data
dat_loss_bin <- data %>%
  select(1:2, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102,107)
View(dat_loss_bin) #190 obs on 20 variables
summary(dat_loss_bin)
#Min.   : 0.000  
#1st Qu.: 4.000  
#Median : 5.000  
#Mean   : 5.503  
#3rd Qu.: 7.000  
#Max.   :11.000  
class(dat_loss_bin$L01_bin) #integer

#count loss modes per country, select columns with loss modes
modes_loss_count <- apply(dat_loss_bin, 1, function(x) length(which(x[3:17]=="1")))
modes_loss_count
class(modes_loss_count)
#convert list into dataframe
modes_loss_count1 <- data.frame(matrix(unlist(modes_loss_count), nrow=190, byrow=TRUE),stringsAsFactors=FALSE)
View(modes_loss_count1)
#rename variable
names(modes_loss_count1)[1] <- "n.loss.modes"
View(modes_loss_count1)
#add rownames
rownames(modes_loss_count1, do.NULL = TRUE, prefix = "row")
rownames(modes_loss_count1) <- dat_loss_bin$iso3
#rename variable
modes_loss_count2 <- cbind(Row.Names = rownames(modes_loss_count1), modes_loss_count1)
#remove row.names
rownames(modes_loss_count2) <- NULL
#rename into iso3
names(modes_loss_count2)[1] <- "iso3"
View(modes_loss_count2) #190 obs
#add democracy
modes_loss_count2 <- cbind(modes_loss_count2, dat_loss_bin$dem_dum)
#rename 
names(modes_loss_count2)[3] <- "dem_dum"
View(modes_loss_count2) #190 obs


#summary stats
summary(modes_loss_count2) #loss 
#Mean = 5.4, Median=5

modes_loss_count2 %>% slice_max(n.loss.modes, n = 3)
#IDN           11
#LTU           11
#NLD           11
modes_loss_count2 %>% slice_min(n.loss.modes, n = 3)
#ECU            0
#URY            0
#ARG            1
#BOL            1
#CPV            1
#CRI            1
#MOZ            1
#PER            1
#PLW            1
#PRK            1

#count number of loss modes per country
#change into long format & remove _bin
modes_loss_count <- dat_loss_bin %>% pivot_longer(names_to = "mode", values_to = "category", L01_bin:L14_bin)
modes_loss_count$mode = sub("\\_bin.*","", as.character(modes_loss_count$mode))
View(modes_loss_count) #2850 obs of 7 variables

#recode values 0 and 1 in whole dataframe
class(modes_loss_count$category)
modes_loss_count$category <- as.character(modes_loss_count$category)
modes_loss_count$category[modes_loss_count$category=='0'] <- 'No'
modes_loss_count$category[modes_loss_count$category=='1'] <- 'Yes'
View(modes_loss_count) #2850 obs on 6 variables


#summary stats by regime
modes_loss_count2 %>%
  group_by(dem_dum) %>%
  summarise_at(vars(n.loss.modes), tibble::lst(mean, median, sd, min, max), na.rm=TRUE)
#dem_dum    mean median    sd   min   max
#Autocracy  5.72      6  2.34     1    10
#Democracy  5.12      5  2.27     0    11


#scatterplot by regime (with regression line)

modes_count <- merge(modes_acq_count2, modes_loss_count2, by=c("iso3", "dem_dum"))
View(modes_count)

#density by regime (Marginal density plot of x )
density_acq <- ggplot(modes_count, aes(n.acq.modes, fill=dem_dum)) + 
  geom_density(alpha=.5) + 
  theme_minimal(base_size = 16)+
  theme(legend.position = "top")+
  guides(fill=guide_legend(""))+
  labs(title = "Acquisition of citizenship (28 modes)",
       subtitle = "Mean (autocracy) =10.2 | Mean (democracy)=12.2")+
  xlab("Number of modes per country")+
  ylab("density")

#scatterplot by regime (Marginal density plot of y )
density_loss <- ggplot(modes_count, aes(n.loss.modes, fill=dem_dum)) + 
  geom_density(alpha=.5) + 
  theme_minimal(base_size = 16)+
  theme(legend.position = "top")+
  guides(fill=guide_legend(""))+
  labs(title = "Loss of citizenship (15 modes)",
       subtitle = "Mean (autocracy) =5.8 | Mean (democracy)=5.3")+
  xlab("Number of modes per country")+
  ylab("density")

#Figure 2
jpeg('Fig2.density_regime.comb_capt.jpeg',  width = 15, height = 8, units = 'in', res = 200)
grid.arrange(arrangeGrob(density_acq, density_loss, nrow=1),
             top = textGrob("",
                            gp=gpar(fontsize=20)),
             bottom = textGrob("",
                               )
)
dev.off()



#Figure 3. A01 birthright combi plot

# Descriptives A01a
data_lb <- data

data_lb$A01a <- factor(data$A01a_cat, levels = c("1","2","3","4","5","0"),
                       labels = c("Generally applicable", "Dual citizenship restriction",  
                                  "Wedlock restriction", "Gender restriction", 
                                  "Group restriction", "No provision"))
View(data_lb)
count(data_lb$A01a)


#A01a description with percentage label
plot_A01a_count <- ggplot(data_lb,aes(x=factor(A01a),fill='blue'))+geom_bar(stat="count")+
  coord_flip()+
  stat_count(geom = "text", colour = "black", size = 6,
             aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.5)+
  geom_text(aes(label=scales::percent(..count../sum(..count..), accuracy= 1)),
            stat='count',position=position_stack(0.5), colour = "red")+
  theme(legend.position='none')+
  theme_minimal(base_size = 18)+
  scale_fill_brewer(drop = F)+
  scale_x_discrete(limits = c("Generally applicable", "Dual citizenship restriction", 
                              "Wedlock restriction", "Gender restriction", 
                              "Group restriction", "[n.a.]", "No provision"))+
  theme(legend.position='none')+
  ylim(0,125)+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  xlab("")+
  ggtitle("A01a. Citizenship by descent (birth in country)")+
  theme(plot.title.position = "plot")+
  ylab("Number of countries with type of provision (% in red)\n")


# Descriptives A01b

data_lb$A01b <- factor(data$A01b_cat, levels = c("1","2","3","4","5","6", "0"),
                       labels = c("Generally applicable", "Dual citizenship restriction",  
                                  "Wedlock restriction", "Gender restriction", 
                                  "Group restriction", "Generational transmission restriction",
                                  "No provision"))
count(data_lb$A01b)

#A01b description with percentage label
plot_A01b_count <- ggplot(data_lb,aes(x=factor(A01b),fill='blue'))+geom_bar(stat="count")+
  coord_flip()+
  stat_count(geom = "text", colour = "black", size = 6,
             aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.5)+
  geom_text(aes(label=scales::percent(..count../sum(..count..), accuracy= 1)),
            stat='count',position=position_stack(0.5), colour = "red")+
  theme(legend.position='none')+
  theme_minimal(base_size = 18)+
  scale_fill_brewer(drop=FALSE) +
  scale_x_discrete(limits = c("Generally applicable", "Dual citizenship restriction", 
                              "Wedlock restriction", "Gender restriction", 
                              "Group restriction", "Generational transmission restriction", "No provision"))+
  theme(legend.position='none')+
  ylim(0,125)+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  xlab("")+
  ggtitle("A01b. Citizenship by descent (birth abroad)")+
  theme(plot.title.position = "plot")+
  ylab("Number of countries with type of provision (% in red)\n")

#combi plot A01a and A01b

jpeg("Fig3.Plot_birthright_descent_capt_nt.jpeg", width = 22, height = 10, units = 'in', res = 100)
grid.arrange(arrangeGrob(plot_A01a_count, plot_A01b_count, nrow=1, widths=c(0.5,0.5)),
             top = textGrob("",gp=gpar(fontsize=28, font =12), just = "center"),
             bottom = textGrob("", just="left")) 
dev.off()


#Figure 4. A06a_yrs
#descriptives
data %>%
  summarise_at(vars(A06a_yrs), tibble::lst(mean, median, sd, min, max), na.rm=TRUE)
#mean     median  sd        min   max
#7.485714      5  4.892943   2  40

plot_A06a_yrs_count_capt <- ggplot(data,aes(x=factor(A06a_yrs),fill=factor(A06a_yrs)))+geom_bar(stat="count")+
  geom_text(aes(label=..count..),stat="count", position=position_stack(0.5))+
  theme_minimal(base_size=18)+
  theme(legend.position='none')+
  ylab("number of countries")+
  xlab("nominal number of years required for residence-based citizenship acquisition\n")+
  labs(title = "",
       subtitle = "",
       caption = "")

jpeg('Fig4. A06a.yrs.count_nt.jpeg',  width = 15, height = 10, units = 'in', res = 100)
plot_A06a_yrs_count_capt
dev.off()


#Figure 5: A02 combi
data_lb$A02 <- ifelse(data$A02a_cat == 0 & data$A02b_cat == 0, "0", #No provision
                      ifelse(data$A02a_cat == 1 , "1", #Generally applicable
                             ifelse(data$A02a_cat == 2 | data$A02b_cat == 1 | data$A02b_cat == 3 , "2", #Parental residence or birth restriction
                                    '3'))) # Gender or group restriction
data_lb$A02 <- factor(data_lb$A02, 
                      labels = c("No provision", "Unconditional", "Parental residence or birth restriction", "Gender or group restriction"))
count(data_lb$A02)

#A02 description with percentage label
plot_A02_count <- ggplot(data_lb,aes(x=factor(A02),fill='blue'))+geom_bar(stat="count")+
  coord_flip()+
  stat_count(geom = "text", colour = "black", size = 6,
             aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.5)+
  geom_text(aes(label=scales::percent(..count../sum(..count..), accuracy= 1)),
            stat='count',position=position_stack(0.5), colour = "red")+
  theme(legend.position='none')+
  theme_minimal(base_size = 18)+
  scale_fill_brewer(drop=FALSE) +
  scale_x_discrete(limits = c("No provision", "Unconditional", 
                              "Parental residence or birth restriction", "Gender or group restriction"))+
  theme(legend.position='none')+
  ylim(0,110)+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  xlab("")+
  theme(plot.title.position = "plot")+
  ylab("Number of countries with type of provision (and % indicated in red)\n")+
  labs(title="", 
       subtitle='',
       caption = '')


jpeg('Fig5.A02.count_nt.jpeg',  width = 15, height = 10, units = 'in', res = 100)
plot_A02_count
dev.off()



#Fig 6. Dualcit plot

data_lb$dualcit_comb <- factor(data$dualcit_comb,
  labels = c("No dual citizenship acceptance", "Only at acquisition abroad", "Only for residence-based acquisition", "Full dual citizenship acceptance"))
count(data_lb$dualcit_comb)

dualcit_count <- ggplot(data_lb,aes(x=factor(dualcit_comb),fill='blue'))+geom_bar(stat="count")+
  coord_flip()+
  stat_count(geom = "text", colour = "black", size = 6,
             aes(label = ..count..),position=position_dodge(width=0.9), vjust=-0.25)+
  geom_text(aes(label=scales::percent(..count../sum(..count..), accuracy= 1)),
            stat='count',position=position_stack(0.5), colour = "red")+
  theme(legend.position='none')+
  theme_minimal(base_size = 18)+
  scale_fill_brewer(drop = F)+
  theme(legend.position='none')+
  ylim(0,125)+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(plot.title.position = "plot")+
  xlab("")+
  ylab("Number of countries with type of provision (% in red)\n")+
  labs(title="", 
   subtitle='',
   caption = '')

jpeg('Fig6.dualcit.count_nt.jpeg',  width = 14, height = 6, units = 'in', res = 100)
dualcit_count
dev.off()



# Figure 7. Liberalism index
# Count group & gender restrictions, no residence-based citizenship, no voluntary renunciation
#A01a, 4 + 5
#A01b, 5 + 6
#A02a, 3
#A02b, 4
#A03a, 3
#A05, 3
#A06, 0
#A06a_yrs >14
#A08, 3,4,5,6,7
#L01, 0, 3, 4

data_illib <- data[,1:2]
View(data_illib)

data_illib$A01a_illib <- ifelse(data$A01a_cat == 4 | data$A01a_cat == 5, "1",
                      0)
data_illib$A01b_illib <- ifelse(data$A01b_cat == 4 | data$A01b_cat == 5, "1",
                      0)
data_illib$A02a_illib <- ifelse(data$A02a_cat == 3 , "1",
                      0)
data_illib$A02b_illib <- ifelse(data$A02b_cat == 4 , "1",
                      0)
data_illib$A03a_illib <- ifelse(data$A03a_cat == 3 , "1",
                      0)
data_illib$A05_illib <- ifelse(data$A05_cat == 3 , "1",
                      0)
data_illib$A06_illib <- ifelse(data$A06_cat == 0, "1",
                      0)
data_illib$A06a_illib <- ifelse(data$A06a_yrs > 14 , "1",
                      0)
data_illib$A08_illib <- ifelse(data$A08_cat == 3 | data$A08_cat == 4 |
                      data$A08_cat == 5 | data$A08_cat == 6 | data$A08_cat == 7, "1",
                      0)
data_illib$L01_illib <- ifelse(data$L01_cat == 0 | data$L01_cat == 3 | data$L01_cat == 4, "1",
                      0)
View(data_illib)


#count number of illiberal modes per country
#generate summary stats table
descr_stats_illib <- data_illib[3:12] %>% 
       egltable( , strict=FALSE)
descr_stats_illib
       
#count illib modes per country, select columns with illib modes
modes_illib_count <- apply(data_illib, 1, function(x) length(which(x[3:12]=="1")))
modes_illib_count
class(modes_illib_count)
#convert list into dataframe
modes_illib_count1 <- data.frame(matrix(unlist(modes_illib_count), nrow=190, byrow=TRUE),stringsAsFactors=FALSE)
View(modes_illib_count1)
#rename variable
names(modes_illib_count1)[1] <- "n.illib.modes"
View(modes_illib_count1)
#add rownames
rownames(modes_illib_count1, do.NULL = TRUE, prefix = "row")
rownames(modes_illib_count1) <- data_illib$iso3
#rename variable
modes_illib_count2 <- cbind(Row.Names = rownames(modes_illib_count1), modes_illib_count1)
#remove row.names
rownames(modes_illib_count2) <- NULL
#rename into iso3
names(modes_illib_count2)[1] <- "iso3"
View(modes_illib_count2) #190 obs
#add democracy
modes_illib_count2 <- cbind(modes_illib_count2, data$dem_dum)
#rename 
names(modes_illib_count2)[3] <- "dem_dum"
View(modes_illib_count2) #190 obs of 4 variables
       
#summary stats
summary(modes_illib_count2)
#Mean = 0.9, Median=0

#Top 10 max
modes_illib_count2 %>% slice_max(n.illib.modes, n = 10)
#   BRN             6 Autocracy
#   LBR             6 Democracy
#   SLE             6 Democracy
#   ARE             5 Autocracy
#   KWT             5 Autocracy
#   NPL             5 Democracy
#   QAT             5 Autocracy
#   BHR             4 Autocracy
#   HTI             4 Autocracy
#  LBN             4 Autocracy
#  LBY             4 Autocracy
#  OMN             4 Autocracy
#  SAU             4 Autocracy

plot_illib_modes_count <- ggplot(modes_illib_count2,aes(x=factor(n.illib.modes),fill=factor(n.illib.modes)))+geom_bar(stat="count")+
  geom_text(stat='count', aes(label=..count..), position=position_stack(0.5), size = 6)+
  theme(legend.position='none')+
       theme_minimal(base_size = 18)+
       theme(strip.text.x = element_text(size = 8, face = "bold", hjust = 0, vjust = -1))+
       theme(legend.position='none')+
       theme(axis.text.x = element_text(size = 14))+
       theme(axis.text.y = element_text(size = 14))+
       xlab("Number of modes with illiberal provisions\n")+
      scale_x_discrete(limits = c("0", "1", "2", "3", "4", "5", "6"))+
       ylab("Number of countries")+
       labs(title = "",
       subtitle = "",
       caption = "")+
       annotate("label", x = 5, y = 90, size = 5, label = "Countries with most modes with illiberal provisions:\n
       Brunei, Liberia, Sierra Leone (6)\n
       UAE, Kuwait, Nepal, Qatar (5)\n
       Bahrain, Haiti, Lebanon, Libya, Oman, Saudi Arabia (4)", hjust=0)
       
       #jpeg
       jpeg('Fig7.illib.modes.count_nt.jpeg',  width = 18, height = 10, units = 'in', res = 200)
       plot_illib_modes_count
       dev.off()
       
