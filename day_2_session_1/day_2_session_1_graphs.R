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
## 230817 BDW  Added examples, data for exercises
#################################################################################

## set working directory, just to be sure
setwd("day_2_session_1")

## create a histogram, boxplot, scatterplot
## load in some sample data (from Scott Emerson, at https://www.emersonstatistics.com/datasets/fev.txt)
fevdat <- read.table("data/fev.txt", header = TRUE)

## clean up the data, based on documentation (https://www.emersonstatistics.com/datasets/fev.doc)
fevdat$smoke <- (-1)*(fevdat$smoke - 2)
fevdat$female <- fevdat$sex - 1

############################################################################
##
## Example plots
##
############################################################################
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

############################################################################
##
## Motivate determining a line
##
############################################################################
lm(fev ~ age, data = fevdat)

## plot a scatterplot of all age vs FEV, with trend line
set.seed(1024)
plotdata <- fevdat
plotdata$age <- fevdat$age + runif(length(fevdat$age), -0.05, 0.05)
mod <- lm(fevdat$fev ~ fevdat$age)

png("figs/fevline.png", width = 600, height = 400)
plot(plotdata$age, plotdata$fev, xlim = c(-1, max(plotdata$age)),
     ylim = c(-1, max(fevdat$fev)), pch = 19, col = "black",
     ylab = "FEV", xlab = "Age (years)", main = "FEV vs age")
abline(mod$coefficients[1], mod$coefficients[2], lwd = 2, col = "blue")
text(x = -1, y = 1, pos = 4, labels = paste("Intercept: ", round(mod$coefficients[1], 2), sep = ""))
text(x = 2, y = 4, pos = 4, labels = paste("Slope: ", round(mod$coefficients[2], 2), sep = ""))
dev.off()

############################################################################
##
## Slopes and intercepts exercise
##
############################################################################

## read in the inflamm data
inflamm <- read.table("data/inflamm.txt", header = TRUE, stringsAsFactors = FALSE)
summary(inflamm$fib)
lm(fib ~ prevdis, data = inflamm)

############################################################################
##
## Reasonable slope and intercept for age vs height
##
############################################################################
lm(height ~ age, data = fevdat)

############################################################################
##
## Shifting the intercept
##
############################################################################
lm(fev ~ I(age + 1), data = fevdat)
lm(fev ~ I(age - 1), data = fevdat)
