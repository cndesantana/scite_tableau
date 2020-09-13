setwd("/Users/isiscosta/Downloads/Biomar_fadmodels/data/")

library(readxl)
library(tidyverse)
library(data.table)
#dat1 <- read_xlsx("Predictions_Rotifers.xlsx")
#dat2 <- read_xlsx("Predictions_Artemia.xlsx")
dat1 <- fread("Predictions_Artemia.csv")
dat2 <- fread("Predictions_Rotifers.csv")


dat1$organism <- "Artemia"
dat2$organism <- "Rotifers"

dat1$company <- "Biomar"
dat2$company <- "Biomar"

nrow(dat1)
nrow(dat2)

dat <- rbind(dat1,dat2)
dat3 <- dat
dat4 <- dat
dat3$company  <- "Competitor-A"
dat4$company <- "Competitor-B"
positions <-  seq(from=6, to=48, by=3)
for(i  in  positions){
  dat1[[i]] <- as.numeric(dat1[[i]])
  dat2[[i]] <- as.numeric(dat2[[i]])
  dat[[i]] <- as.numeric(dat[[i]])
  dat3[[i]] <- as.numeric(dat3[[i]])
  dat4[[i]] <- as.numeric(dat4[[i]])
  dat3[[i]] <- as.numeric(quantile(dat1[[i]], probs = 0.4))
  dat4[[i]] <- as.numeric(quantile(dat2[[i]], probs = 0.3))
}

dat <- rbind(dat,dat3)
dat <- rbind(dat,dat4)

options(digits=12)
dat %>% select(Time, 
               Dose, 
               Density, 
               Temperature, 
               organism,
               company,
               mean_ARA_mg,
               mean_EPA_mg,
               mean_DHA_mg,
               mean_EPA_ARA_mg,
               mean_DHA_EPA_mg,
               mean_ARA_PL,
               mean_EPA_PL,
               mean_DHA_PL,
               mean_EPA_ARA_PL,
               mean_DHA_EPA_PL,
               mean_ARA_TL,
               mean_EPA_TL,
               mean_DHA_TL,
               mean_EPA_ARA_TL,
               mean_DHA_EPA_TL
               ) %>%
  mutate(mean_ARA_mg = format(as.numeric(mean_ARA_mg), decimal.mark="."),
         mean_EPA_mg = format(as.numeric(mean_EPA_mg), decimal.mark="."),
         mean_DHA_mg = format(as.numeric(mean_DHA_mg), decimal.mark="."),
         mean_EPA_ARA_mg = format(as.numeric(mean_EPA_ARA_mg), decimal.mark="."),
         mean_DHA_EPA_mg = format(as.numeric(mean_DHA_EPA_mg), decimal.mark="."),
         mean_ARA_PL = format(as.numeric(mean_ARA_PL), decimal.mark="."),
         mean_EPA_PL = format(as.numeric(mean_EPA_PL), decimal.mark="."),
         mean_DHA_PL = format(as.numeric(mean_DHA_PL), decimal.mark="."),
         mean_EPA_ARA_PL = format(as.numeric(mean_EPA_ARA_PL), decimal.mark="."),
         mean_DHA_EPA_PL = format(as.numeric(mean_DHA_EPA_PL), decimal.mark="."),
         mean_ARA_TL = format(as.numeric(mean_ARA_TL), decimal.mark="."),
         mean_EPA_TL = format(as.numeric(mean_EPA_TL), decimal.mark="."),
         mean_DHA_TL = format(as.numeric(mean_DHA_TL), decimal.mark="."),
         mean_EPA_ARA_TL = format(as.numeric(mean_EPA_ARA_TL), decimal.mark="."),
         mean_DHA_EPA_TL = format(as.numeric(mean_DHA_EPA_TL), decimal.mark="."),
         Link = "Link") %>%
  gather(Dimension, Measure, mean_ARA_mg:mean_DHA_EPA_TL, factor_key=TRUE) %>%
  mutate(type = case_when(Dimension %in% c("mean_ARA_mg","mean_EPA_mg","mean_DHA_mg","mean_EPA_ARA_mg","mean_DHA_EPA_mg") ~ "mg",
                          Dimension %in% c("mean_ARA_PL","mean_EPA_PL","mean_DHA_PL","mean_EPA_ARA_PL","mean_DHA_EPA_PL") ~ "PL",
                          Dimension %in% c("mean_ARA_TL","mean_EPA_TL","mean_DHA_TL","mean_EPA_ARA_TL","mean_DHA_EPA_TL") ~ "TL")) %>%
  mutate(fatty = case_when(Dimension %in% c("mean_ARA_mg","mean_ARA_PL","mean_ARA_TL") ~ "ARA",
                          Dimension %in% c("mean_EPA_mg","mean_EPA_PL","mean_EPA_TL") ~ "EPA",
                          Dimension %in% c("mean_DHA_mg","mean_DHA_PL","mean_DHA_TL") ~ "DHA",
                          Dimension %in% c("mean_EPA_ARA_mg","mean_EPA_ARA_PL","mean_EPA_ARA_TL") ~ "EPA:ARA",
                          Dimension %in% c("mean_DHA_EPA_mg","mean_DHA_EPA_PL","mean_DHA_EPA_TL") ~ "DHA:EPA")) %>%
  mutate(Measure = as.numeric(Measure)) %>%
  mutate(Measure = if_else(Measure < 0, 0.0001, Measure)) %>%
  group_by(type, fatty, organism) %>%
  mutate(maxim = max(Measure))%>%
  ungroup()%>%
  dplyr::mutate(prop = as.numeric(Measure)/as.numeric(maxim))%>%
  openxlsx::write.xlsx("/Users/isiscosta/Documents/Radar plot_Radar plot.xlsx", dec = ".")


library(readxl)

test <- read_xlsx("radar plot.xlsx")
head(test)
table(test$type)
test %>% filter(Time == 30, Dose == 1, Density == 300, Temperature == 24, company == "Biomar")
test %>% filter(Time == 30, Dose == 1, Density == 300, Temperature == 24, company == "Competitor-A")
test %>% filter(Time == 30, Dose == 1, Density == 300, Temperature == 28, company == "Biomar")
test %>% filter(Time == 30, Dose == 1, Density == 300, Temperature == 28, company == "Competitor-A")



test$Measure <- as.numeric(test$Measure)
test$maxim <- as.numeric(test$maxim)

test %>% filter(Measure < 0)
