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
##
#################################################################################

## set working directory, just to be sure
setwd("P://Epi-Biost-Workshop2016-master//Epi-Biost-Workshop2016-master//Epi-Biost-Workshop2016")

## create a histogram, boxplot, scatterplot
## load in some sample data
leprosy <- read.table("C://Users//brianw26//Dropbox//Courses//2015-2016 Second Year//Q2 Winter//BIOST 571//HW02//leprosy.txt", header = TRUE)

## histogram of count1
png("day_2_graphs_hist_example.png", width = 400, height = 300)
par(mar=c(5.1, 4.1, 4.1, 0.1), cex.lab = 1.45)
hist(leprosy$count1, main = "", xlab = "Baseline counts of leprosy bacilli", ylab = "Frequency")
dev.off()

## boxplot of count1 by severe or not
png("day_2_graphs_bplot_example.png", width = 400, height = 300)
par(mar=c(5.1, 4.1, 4.1, 0.1), cex.lab = 1.5, cex.axis = 1.25)
boxplot(leprosy$count1 ~ leprosy$severe, xlab = "Severity", ylab = "Baseline counts of bacilli", axes = FALSE)
axis(side = 2, at = seq(0, max(leprosy$count1) + 5, by = 5))
axis(side = 1, at = c(1, 2), labels = c("Non-severe", "Severe"))
box()
dev.off()

## scatterplot of count1 vs count2, colored by treatment group
sub.1 <- subset(leprosy, leprosy$trt == 1)
sub.2 <- subset(leprosy, leprosy$trt == 2)
sub.3 <- subset(leprosy, leprosy$trt == 3)

png("day_2_graphs_scatplot_example.png", width = 400, height = 300)
par(mar=c(5.1, 4.1, 4.1, 0.1), cex.lab = 1.5)
plot(sub.1$count1, sub.1$count2, main = "", ylab = "Count of bacilli at time 1", xlab = "Baseline count of bacilli",
     col = "black", pch = 19, xlim = c(min(leprosy$count1), max(leprosy$count1)),
     ylim = c(min(leprosy$count2), max(leprosy$count2)))
points(sub.2$count1, sub.2$count2, pch = 18, col = "blue")
points(sub.3$count1, sub.3$count2, pch = 17, col = "red")
legend(2.5, 23, legend = c("Treatment 1", "Treatment 2", "Treatment 3"), col = c("black", "blue", "red"),
       pch = c(19, 18, 17), bty = "n", cex = 1.25)
lines(sub.1$count1, fitted(lm(sub.1$count2 ~ sub.1$count1)))
lines(sub.2$count1, fitted(lm(sub.2$count2 ~ sub.2$count1)), col = "blue")
lines(sub.3$count1, fitted(lm(sub.3$count2 ~ sub.3$count1)), col = "red")
dev.off()
