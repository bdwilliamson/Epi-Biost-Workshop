#################################################################################
##
## FILE: day_2_session_1_graphs.R
##
## PURPOSE: Code to create plots used in the slides.
##
## CREATED: 1 September 2016
##
## AUTHOR: Brian Williamson
##
## INPUTS: None
##
## OUTPUTS:
##
## UPDATES:
## MMDDYY INIT COMMENTS
## ------ ---- --------
## 220817 BDW  Set new working directory; remove line example; motivate all sections
##             in slides using FEV data, not leprosy data
#################################################################################

## set working directory, just to be sure
setwd("day_2_session_1")

## create a histogram, boxplot, scatterplot
## load in some sample data (from Scott Emerson, at https://www.emersonstatistics.com/datasets/fev.txt)
fevdat <- read.table("data/fev.txt", header = TRUE)

## clean up the data, based on documentation (https://www.emersonstatistics.com/datasets/fev.doc)
fevdat$smoke <- (-1)*(fevdat$smoke - 2)
fevdat$female <- fevdat$sex - 1

## histogram of fev
png("figs/day_2_graphs_hist_example.png", width = 400, height = 300)
par(mar=c(5.1, 4.1, 4.1, 0.1), cex.lab = 1.45)
hist(fevdat$fev, main = "", xlab = "FEV", ylab = "Frequency")
dev.off()

## boxplot of count1 by severe or not
png("figs/day_2_graphs_bplot_example.png", width = 400, height = 300)
par(mar=c(5.1, 4.1, 4.1, 0.1), cex.lab = 1.5, cex.axis = 1.25)
boxplot(fevdat$fev ~ fevdat$smoke, xlab = "Smoking status", ylab = "FEV", axes = FALSE)
axis(side = 2, at = seq(0, max(fevdat$fev) + 5, by = 1))
axis(side = 1, at = c(1, 2), labels = c("Non-smoker", "Smoker"))
box()
dev.off()

## scatterplot of fev vs age, colored by sex
sub.1 <- subset(fevdat, fevdat$female == 0)
sub.2 <- subset(fevdat, fevdat$female == 1)

## jitter the data on the x-axis to make it a bit easier to see
set.seed(4747)
sub.1.plotdata <- sub.1
sub.1.plotdata$age <- sub.1$age + runif(length(sub.1$age), -0.05, 0.05)
set.seed(5555)
sub.2.plotdata <- sub.2
sub.2.plotdata$age <- sub.2$age + runif(length(sub.2$age), -0.05, 0.05)

png("figs/day_2_graphs_scatplot_example.png", width = 400, height = 300)
par(mar = c(5.1, 4.1, 4.1, 0.1), cex.lab = 1.5)
plot(sub.1.plotdata$age, sub.1$fev, main = "", ylab = "FEV", xlab = "Age",
     col = "black", pch = 19, xlim = c(min(fevdat$age), max(fevdat$age)),
     ylim = c(min(fevdat$fev), max(fevdat$fev)))
points(sub.2.plotdata$age, sub.2$fev, pch = 18, col = "blue")
legend("topleft", legend = c("Male", "Female"), col = c("black", "blue"),
       pch = c(19, 18), cex = 1.25)
lines(sub.1$age, fitted(lm(sub.1$fev ~ sub.1$age)))
lines(sub.2$age, fitted(lm(sub.2$fev ~ sub.2$age)), col = "blue")
dev.off()
