#######################################################################################################################
##
## FILE: day_3_practice.R
##
## AUTHOR: Brian Williamson
## 
## CREATED: 21 September 2016
##
## PURPOSE: Practice writing a script via a data analysis, for EPI/BIOST bootcamp 2016
## 
## INPUTS: mri data - either from uwIntroStats or from http://www.emersonstatistics.com/datasets/mri.txt
##
## OUTPUTS: analysis.Rdata - mri dataset with all rows containing any NAs removed
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
## 
#######################################################################################################################

## set working directory
setwd("~/Workshop2016-master/day_3_session_1")

## load uwIntroStats, and mri data
library(uwIntroStats)
# data(mri) ## could use this, but use read.table to practice
mri <- read.table("mri.txt", header = TRUE, stringsAsFactors = FALSE)


##############################################################
##
## First task: create a BMI variable, 
## 
##############################################################
## look at the data
View(mri)

## describe the whole dataset, using descrip
descrip(mri)

## describe only age
descrip(mri$age, strata = mri$male)

## create a new variable called bmi, add to mri
## first note that weight is stored in pounds, not kg (find this by ?mri)
## so multiply weight by approx .45 kg per pound
## height in cm, so divide by 100 and then square to get denominator
mri$bmi <- (mri$weight*0.453592)/((mri$height/100) ^ 2)

## check: does it make sense?
head(mri$bmi)
descrip(mri$bmi)

## check for missing data
sum(is.na(mri))

## 55 values: where are they?
## look at output of descrip again, more closely
descrip(mri)

## missing values in packyrs, ldl, alb, crt, plt, aai, fev, dsst, whgrd, volinf
## most common to miss ldl, fev, or dsst

## are some the same ptid?
## get all rows with missing data
miss <- rbind(mri[is.na(mri$packyrs), ],
              mri[is.na(mri$ldl), ],
              mri[is.na(mri$alb), ],
              mri[is.na(mri$crt), ],
              mri[is.na(mri$plt), ],
              mri[is.na(mri$aai), ],
              mri[is.na(mri$fev), ],
              mri[is.na(mri$dsst), ],
              mri[is.na(mri$whgrd), ],
              mri[is.na(mri$volinf), ])
## order by ptid
miss <- miss[order(miss$ptid), ]

## count the number of unique, compare to total
length(unique(miss$ptid))
length(miss$ptid)

## only 7 people are missing multiple values
## only 48 people missing values out of the 735, or
48/735
## approx 6%. Might be justified in complete case analyses (only analyze those without missing data)

## remove all rows with any missing data
analysis <- na.omit(mri)

## save in the folder!
save(analysis, file = "analysis_data.Rdata")
## stop the walkthrough here

## describe the whole data (note that we would have to clean this up for a paper/homework)
descrip(mri)

## get only females, describe them
female <- subset(mri, mri$male == 0)
descrip(female)

## scatterplot of atrophy vs age
scatter(mri$atrophy, mri$age, main = "Cerebral Atrophy vs Age", ylab = "Cerebral Atrophy", 
        xlab = "Age (years)")

## same scatterplot stratified by sex
scatter(mri$atrophy, mri$age, strata = mri$male, main = "Cerebral Atrophy vs Age, by Sex", 
        ylab = "Cerebral Atrophy", xlab = "Age (years)")
