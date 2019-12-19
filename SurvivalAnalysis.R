library(dplyr)
library(tidyr)
library(survival)
library(survminer)


##https://rpkgs.datanovia.com/survminer/index.html


nypd <- read.csv("//storage/Projects/Mat/STA310-Kuiper/Final Project/4_LeeKozikJindal_NYPD/FinalData.csv")

#########################################################

### race by year analysis ###
nypd.surv <- select(nypd, SUSPECT_ARRESTED_FLAG, FRISKED_FLAG, SEARCHED_FLAG, STOP_DURATION_MINUTES, SUSPECT_RACE_DESCRIPTION, YEAR2)

##simpilfied race
nypd.surv$SIMPLE_RACE <- ifelse(nypd.surv$SUSPECT_RACE_DESCRIPTION == "Black", "Black",
                                ifelse(nypd.surv$SUSPECT_RACE_DESCRIPTION == "Black Hispanic" | nypd.surv$SUSPECT_RACE_DESCRIPTION == "White Hispanic", "Hispanic",
                                       ifelse(nypd.surv$SUSPECT_RACE_DESCRIPTION == "White", "White", "Other")))

##simplified year before and after cour decision
nypd.surv <- filter(nypd.surv, !is.na(nypd.surv$YEAR2))
nypd.surv$AFTER2012 <- ifelse(nypd.surv$YEAR <= 2012, "Before", "After")

##Include only Black, hispanic and white for race & Before or After for year (no NA)
nypd.surv <- filter(nypd.surv, SIMPLE_RACE == "Black"| SIMPLE_RACE == "Hispanic" |SIMPLE_RACE == "White")
nypd.surv <- filter(nypd.surv, AFTER2012 == "Before" | AFTER2012 == "After")

##ANALYSIS
#censor past 50 minutes
nypd.surv$FRISKED_FLAG[nypd.surv$STOP_DURATION_MINUTES>50] <- 0
nypd.surv$STOP_DURATION_MINUTES[nypd.surv$STOP_DURATION_MINUTES>50] <- 50
##log rank test for race and year (separate)

survdiff(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SIMPLE_RACE,data=nypd.surv, SIMPLE_RACE=="Black"|SIMPLE_RACE=="Hispanic")
survdiff(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~AFTER2012,data=nypd.surv)

## new column for year and race
nypd.surv <- unite(nypd.surv,RACE_BY_YEAR, c(SIMPLE_RACE, AFTER2012))


#Run surval analysis
KM.FRISKED <- survfit(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~RACE_BY_YEAR,data=nypd.surv)
ggsurvplot(KM.FRISKED, data = nypd.surv, palette = c("steelblue4", "steelblue1", "seagreen4", "seagreen1", "plum4", "plum1"), legend.lab = c("Black After", "Black Before", "Hispanic After", "Hispanic Before", "White After", "White Before"), ggtheme = theme_bw()) + labs(
  title    = "Frisk Survival by Race Before or After 2013 court decision")
##change colors ## log ranks with graphic

survdiff(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SIMPLE_RACE,data=nypd.surv)

#####################################





nypd.surv <- select(nypd, SUSPECT_ARRESTED_FLAG, FRISKED_FLAG, SEARCHED_FLAG, STOP_DURATION_MINUTES, SUSPECT_RACE_DESCRIPTION)

##censor stop time of more than 200

nypd.surv$FRISKED_FLAG[nypd.surv$STOP_DURATION_MINUTES>50] <- 0
nypd.surv$SEARCHED_FLAG[nypd.surv$STOP_DURATION_MINUTES>200] <- 0
nypd.surv$SUSPECT_ARRESTED_FLAG[nypd.surv$STOP_DURATION_MINUTES>200] <- 0
nypd.surv$STOP_DURATION_MINUTES[nypd.surv$STOP_DURATION_MINUTES>200] <- 200

##Kaplan Meier Analysis for Arrest
KM.Arrest <- survfit(Surv(STOP_DURATION_MINUTES, SUSPECT_ARRESTED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.surv)

ggsurvplot(KM.Arrest, data = nypd.surv, legend.labs = c("American Indian", "Asian", "Black", "Black Hispanic", "Other", "White", "White Hispanic"),risk.table = TRUE, risk.table.col = "strata" ,  ggtheme = theme_bw()) + labs(
  title    = "Survival Curve for Arrest")

survdiff(Surv(STOP_DURATION_MINUTES, SUSPECT_ARRESTED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.surv, SUSPECT_RACE_DESCRIPTION=="Black"|SUSPECT_RACE_DESCRIPTION=="Black Hispanic"|SUSPECT_RACE_DESCRIPTION=="White Hispanic")


##Kaplan Meier Analysis for Searched
KM.SEARCHED <- survfit(Surv(STOP_DURATION_MINUTES, SEARCHED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.surv)


ggsurvplot(KM.SEARCHED, data = nypd.surv, legend.labs = c("American Indian", "Asian", "Black", "Black Hispanic", "Other", "White", "White Hispanic"),risk.table = TRUE, risk.table.col = "strata" ,  ggtheme = theme_bw()) + labs(
  title    = "Survival Curve for Search")
survdiff(Surv(STOP_DURATION_MINUTES, SEARCHED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.surv, SUSPECT_RACE_DESCRIPTION=="Black"|SUSPECT_RACE_DESCRIPTION=="Black Hispanic"|SUSPECT_RACE_DESCRIPTION=="White Hispanic")

##Kaplan Meier Analysis for Frisked

nypd.surv$STOP_DURATION_MINUTES[nypd.surv$STOP_DURATION_MINUTES>50] <- 50
KM.FRISKED <- survfit(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.surv)

ggsurvplot(KM.FRISKED, data = nypd.surv, legend.labs = c("American Indian", "Asian", "Black", "Black Hispanic", "Other", "White", "White Hispanic"),risk.table = TRUE, risk.table.col = "strata" ,  ggtheme = theme_bw()) + labs(
  title    = "Survival Curve for Frisk")

survdiff(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.surv, SUSPECT_RACE_DESCRIPTION=="Black"|SUSPECT_RACE_DESCRIPTION=="Black Hispanic"|SUSPECT_RACE_DESCRIPTION=="White Hispanic")

##SPLIT into before and after 2012
nypd.before2012 <- filter(nypd.surv, nypd$YEAR <= 2012)
nypd.after2012 <- filter(nypd.surv, nypd$YEAR > 2012)

## new column for year and race
nypd.surv$AFTER2012 <- ifelse(nypd$YEAR <= 2012, "Before", "After")
nypd.surv <- unite(nypd.surv,RACE_BY_YEAR, c(SUSPECT_RACE_DESCRIPTION, AFTER2012), )

##run frisked for before 

KM.FRISKED.before <- survfit(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.before2012)

ggsurvplot(KM.FRISKED.before, data = nypd.surv, legend.labs = c("American Indian", "Asian", "Black", "Black Hispanic", "Other", "White", "White Hispanic"),risk.table = TRUE, risk.table.col = "strata" ,  ggtheme = theme_bw()) + labs(
  title    = "Survival Curve for Frisk before 2012")

survdiff(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.before2012, SUSPECT_RACE_DESCRIPTION=="Black"|SUSPECT_RACE_DESCRIPTION=="Black Hispanic"|SUSPECT_RACE_DESCRIPTION=="White Hispanic")

##run frisked for after 2012

KM.FRISKED.after <- survfit(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.after2012)

ggsurvplot(KM.FRISKED.after, data = nypd.surv, legend.labs = c("American Indian", "Asian", "Black", "Black Hispanic", "Other", "White", "White Hispanic"),risk.table = TRUE, risk.table.col = "strata" ,  ggtheme = theme_bw()) + labs(
  title    = "Survival Curve for Frisk after 2012")

survdiff(Surv(STOP_DURATION_MINUTES, FRISKED_FLAG)~SUSPECT_RACE_DESCRIPTION,data=nypd.after2012, SUSPECT_RACE_DESCRIPTION=="Black"|SUSPECT_RACE_DESCRIPTION=="Black Hispanic"|SUSPECT_RACE_DESCRIPTION=="White Hispanic")




