#John Spaw
#Create data for input into JAGS
#Combining data from 2010 census, 2016 CCES survey, and 2016 IAT Responses

library(mrpdata) #loading census data
library(reshape) #combine_factor
library(dplyr) 

#### DATA LOAD ####
#Load state level IAT data 
state_IAT <- readRDS("Data/state_IAT.rds")
state_IAT <- subset(state_IAT, state_IAT$state_code != "DC") #remove DC

#Load CCES survey data 
load("Data/CCES16_Common_OUTPUT_Feb2018_VV.RData") #loads as "x"
CCES.complete <- x
rm("x") #remove initial load to save memory

#Load census datasets for other US applications of MRP:
data(mrp.census)   # census with common demo strata
mrp.census <- na.omit(mrp.census)

##State code/name key table
state_codes <- read.csv("Data/state_codes.csv")

############################

### Clean data - make variables consistent ###
# Need to match strata between CCES poll, census population, and State IAT data
# Convert variables to factors with identical levels
# Include: Education, Gender, Race, State



###Education
education_levels <- c("No High School", "High School", "Some College", "College Graduate", "Post Graduate")

##CCES
#Cleaning
CCES.complete$educ[CCES.complete$educ == "2-year"] <- "Some college" #combine some college and 2 - year
CCES.complete <- subset(CCES.complete, CCES.complete$educ != "Skipped" & CCES.complete$educ != "Not Asked") #remove NAs

#Combine and rename levels
CCES.complete$education <- combine_factor(CCES.complete$educ, c(1,2,3,3,4,5,1,1)) 
levels(CCES.complete$education) <- education_levels

##Census
#No cleaning necessary... just rename levels
levels(mrp.census$education) <- education_levels



###Race
race_levels <- c("White", "Black", "Hispanic", "Other")

##CCES
#Combine and rename levels
CCES.complete$race <- combine_factor(CCES.complete$race, c(1,2,3,4,4,4,4,4,4,4)) 
levels(CCES.complete$race) <- race_levels

##Census
#Rename levels
levels(mrp.census$race) <- race_levels



## To ensure matching of strata between poll and population,
## both should be factors with identical names and levels.
CCES.complete$sex <- CCES.complete$gender
CCES.complete <- within (CCES.complete, {
  female <- factor(sex=="Female", labels=c("Male","Female"), exclude=NA)
  race <- factor(race,exclude=NA)
  f.race <- interaction(female,race)
})

## Poll has four levels of education, so we need to combine
## the top two levels in the census data. We'll also go ahead
## and trim it down to just the variables used here.
mrp.census <- within(mrp.census,{
  state <- factor(state,exclude=NA)
  race <- factor(race,exclude=NA)
  f.race <- interaction(sex,race)
})



###State
CCES.complete$inputstate <- as.character(CCES.complete$inputstate)
state_codes$State <- as.character(state_codes$State)
state_codes$Abbreviation <- as.character(state_codes$Abbreviation) 

##CCES
CCES.complete$State <- factor(CCES.complete$inputstate)

##Census
#change data type for merge
mrp.census <- left_join(mrp.census, state_codes, by = c("state" = "Abbreviation"))
mrp.census$State <- factor(mrp.census$State)

##IAT
state_IAT <- left_join(state_IAT, state_codes, by = c("state_code" = "Abbreviation"))
state_IAT$State <- factor(state_IAT$State)

##Remove District of Columbia from data
mrp.census <- subset(mrp.census, mrp.census$State != "District of Columbia")
CCES.complete <- subset(CCES.complete, CCES.complete$State != "District of Columbia")
#Remove District of Columbia from levels
mrp.census$State <- factor(mrp.census$State)
CCES.complete$State <- factor(CCES.complete$State)



###Model responses
#Help dreamers
CCES.complete$issue_help_dreamer <- as.factor(CCES.complete$CC16_331_3)
CCES.complete$issue_help_dreamer <- combine_factor(CCES.complete$issue_help_dreamer, c(1,2,1,1))
levels(CCES.complete$issue_help_dreamer) <- c("Help Dreamers", "Don't Help Dreamers")
table(CCES.complete$issue_help_dreamer)

#Deport undocumented immigrants
CCES.complete$issue_deport_undocumented <- as.factor(CCES.complete$CC16_331_7)
CCES.complete$issue_deport_undocumented <- combine_factor(CCES.complete$issue_deport_undocumented, c(1,2,1,1))
levels(CCES.complete$issue_deport_undocumented) <- c("Deport Undocumented Immigrants", "Don't Deport Undocumented Immigrants")
table(CCES.complete$issue_deport_undocumented)



#######Check that all levels match
levels(mrp.census$State) == levels(CCES.complete$State) & levels(mrp.census$State) == levels(state_IAT$State)
levels(mrp.census$f.race) == levels(CCES.complete$f.race)
levels(mrp.census$education) == levels(CCES.complete$education)

############################

### CREATE JAGS DATA ###
mrp.formula <- issue_help_dreamer ~ f.race + education
mrp.frame <- model.frame(mrp.formula, strata=State, data=CCES.complete)
data <- list()
data$y <- model.response(mrp.frame)
data$x <- model.matrix(mrp.frame, mrp.frame)
data$state <- model.extract(mrp.frame, "strata")
data$num_x <- ncol(data$x)
data$num_y <- length(data$y)

predictors <- c("State", attr(terms(mrp.frame), "term.labels"))
data.census <- aggregate(list(weights=mrp.census$weighted2008),
                         as.list(mrp.census[,predictors]),
                         sum)
data.census <- reshape(data.census, 
                       varying=list(State=levels(data$state)), 
                       v.names="weights", 
                       timevar="State", 
                       times=levels(data$state), 
                       idvar=setdiff(predictors,"State"), 
                       direction="wide")
data$pred_x <- model.matrix(update(mrp.formula, NULL ~ .), data.census)
data$census_count <- as.matrix(data.census[,levels(data$state)])
data$num_cells <- nrow(data$census_count)
data$num_state <- ncol(data$census_count)



