library(haven)
library(datasets)
library(readr)
library(dplyr)

#IAT data
IAT_2015 <- read_sav("Data/Race IAT.public.2015.sav")
IAT_2015_reduced <- subset(IAT_2015, IAT_2015$STATE %in% state.abb) #only consider observations from the 50 states
n_iat <- 322950

#state data
us_pop <- 308745538

state_pop <- read_csv("~/Google Drive/Graduate/Spring 18/PS919/Final_Project_Bayes/Data/pop_2015.csv")
state_codes <- read_csv("Data/state_codes.csv")

state_pop <- left_join(state_pop, state_codes, by = c("GEO.display-label" = "State")) #add state codes
state_pop <- data.frame(state_pop$Abbreviation, state_pop$respop72015) #start fresh with code and 2015 pop
state_pop <- state_pop[-c(1,10,53), ]
rownames(state_pop) <- state_codes$Abbreviation[-c(9)]

#computed columns
state_pop$pop_proportion <- state_pop$state_pop.respop72015/us_pop
state_pop$iat_n <- table(IAT_2015_reduced$STATE)
state_pop$IAT_proportion <- round(table(IAT_2015_reduced$STATE)/nrow(IAT_2015_reduced),8)
iat_mean <- aggregate(IAT_2015_reduced$D_biep.White_Good_all, by = list(IAT_2015_reduced$STATE), mean, na.rm=TRUE)
state_pop$IAT_mean <- iat_mean$x
iat_median <- aggregate(IAT_2015_reduced$D_biep.White_Good_all, by = list(IAT_2015_reduced$STATE), median, na.rm=TRUE)
state_pop$IAT_median <- iat_median$x
iat_var <- aggregate(IAT_2015_reduced$D_biep.White_Good_all, by = list(IAT_2015_reduced$STATE), var, na.rm=TRUE)
state_pop$IAT_var <- iat_var$x
state_pop$IAT_pop_diff <- state_pop$IAT_proportion - state_pop$pop_proportion

#rename columns
names(state_pop) <- c("state_code","pop_2015","pop_proportion","IAT_n","IAT_proportion",
                      "IAT_mean","IAT_median","IAT_var","IAT_pop_diff")

#export
saveRDS(state_pop, "Data/state_IAT.rds")

#clean environment
rm(list=ls())
