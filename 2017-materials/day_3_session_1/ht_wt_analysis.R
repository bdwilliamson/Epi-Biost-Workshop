##########################################################################################
##
## FILE: ht_wt_analysis.R
##
## CREATED: 26 September 2017
##
## PURPOSE: introduce people to R
##
## INPUTS: no inputs (yet)
##
## OUTPUTS: no outputs (yet)
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
##########################################################################################

## assign height and weight values
ht <- c(72, 65, 84, 73, 68)
wt <- c(165, 120, 210, 180, 125)

## calculate the median of ht and assign it to quantile.ht
quantile.ht <- quantile(x = ht, probs = 0.5)

## read in data
ht_wt_data <- read.table("~/Documents/Teaching/Epi-Biost-Workshop/day_3_session_1/data/ht_wt_data.txt", sep = "", header = TRUE)

## save off new data
# write.table(ht_wt_data, "~/Documents/Teaching/Epi-Biost-Workshop/day_3_session_1/data/ht_wt_data_2.txt")

## access the first value of ht
ht[1]

## access a range of values in ht
ht[1:3]

## access the first, third, fifth value in ht
ht[c(1, 3, 5)]

## 
ht[c(1, 3:4, 5)]

## which values are greater than 70
ht > 70

## get the subset with ht > 70
ht[ht > 70]


## read in the FEV data
fevdat <- read.table("http://www.emersonstatistics.com/datasets/fev.txt", sep = "", header = TRUE)

## look at the whole data
View(fevdat)

## look at the first few observations
head(fevdat)

## equivalently look at the first few observations
fevdat[1:6, ]

## look at all heights > 70
fevdat$height > 70

## how many are greater than 70
sum(fevdat$height > 70)

## subset the data
greater.70 <- fevdat[fevdat$height > 70, ]
head(greater.70)

## save the subset
write.table(greater.70, "~/Documents/Teaching/Epi-Biost-Workshop/day_3_session_1/data/greater_70_fev.txt")

## look at the first column, 1st row
ht_wt_data[1, 1]
## first observation
ht_wt_data[1, ]
## first column, all observations
ht_wt_data[, 1]
ht_wt_data$height

## logical expressions
ht_wt_data$height > 70
ht_wt_data$height > 70 | ht_wt_data$height > 60
ht_wt_data$height > 70 & ht_wt_data$weight > 160
!(ht_wt_data$height > 70)

ht_wt_data$height == 70
View(ht_wt_data)

## make new variable; call it female
fevdat$female <- fevdat$sex - 1

## look at all of the names
names(fevdat)
str(fevdat)

## scatterplot of fev vs age
plot(fevdat$age, fevdat$fev, main = "FEV vs age", xlab = "Age (years)", ylab = "FEV")

## pull up the help file for quantile()
?quantile
?plot
hist(fevdat$fev)
?hist
