# wd <- "SET WORKING DIRECTORY HERE"
wd <- "C:/Users/chjh/Dropbox/projects/2014rpp/master"

######################################################################################
# Master script Tilburg University Analyses                                          #
# Responsible for this file: CHJ Hartgerink (c.h.j.hartgerink@tilburguniversity.edu) #
######################################################################################

setwd(wd)
# Read in Tilburg data
MASTER <- read.csv('tilburg data.csv')
# source functions
source('functions.r')
library(Hmisc)

# ALL ANALYSES INCLUDE PAIRWISE SELECTION.

#-----------------------------

# Writing out data for table
# https://docs.google.com/spreadsheets/d/16aqIekNerZcflSJwM7dOiXnN24-KvZqaa3eTg4js41U/edit#gid=0
# Columns B-E
### Recode "Publishing journal and subdiscipline"
jour <- numeric()

for(i in 1:nrow(MASTER)) {
  if(as.character(MASTER$Journal..O.[i]) == "JEPLMC") {
    jour[i] <- 1
  } else if(as.character(MASTER$Journal..O.[i]) == "JPSP") {
    jour[i] <- 2
  } else if(as.character(MASTER$Journal..O.[i]) == "PS") {
    if(as.character(MASTER$Discipline..O.[i]) == "Cognitive") {
      jour[i] <- 3
    } else if(as.character(MASTER$Discipline..O.[i]) == "Social") {
      jour[i] <- 4
    } else { jour[i] <- 5 }
  }
  else { jour[i] <- NA }
}

# Overall 
sel <- MASTER[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.),]

# Column B
# Relations tested
reltest <- dim(sel)[1]
# Relations both significant
relsig <- sum((sel$T_pval_USE..O.) <= .05 & (sel$T_pval_USE..R. <= .05), na.rm = TRUE)
writeClipboard(
  paste0(
    round(relsig, 0),
    " / ",
    round(reltest, 0)
  )
)
cat("\014")
readline("Paste into column B, then press ENTER key")

# Column C
# Percent
writeClipboard(as.character(round((relsig / reltest), 2)))
cat("\014")
readline("Paste into column C, then press ENTER key")

# Column D
# Mean
temp1 <- mean(sel$T_r..O., na.rm = TRUE)
# SD
temp2 <- sd(sel$T_r..O., na.rm = TRUE)

writeClipboard(paste0(round(temp1, 2), " (", round(temp2, 2), ")"))
cat("\014")
readline("Paste into column D, then press ENTER key")

# Column E
# Mean
temp1 <- mean(sel$T_r..R., na.rm = TRUE)
# SD
temp2 <- sd(sel$T_r..R., na.rm = TRUE)

writeClipboard(paste0(round(temp1, 2), " (", round(temp2, 2), ")"))
cat("\014")
readline("Paste into column E, then press ENTER key")

# Per journal 
for(journal in c(2,1,4,3,5)){

  sel <- MASTER[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.) & jour == journal,]
  
  # Column B
  # Relations tested
  reltest <- dim(sel)[1]
  # Relations both significant
  relsig <- sum((sel$T_pval_USE..O.) <= .05 & (sel$T_pval_USE..R. <= .05), na.rm = TRUE)
  writeClipboard(
    paste0(
      round(relsig, 0),
      " / ",
      round(reltest, 0)
    )
  )
  cat("\014")
  readline("Paste into column B, then press ENTER key")
  
  # Column C
  # Percent
  writeClipboard(as.character(round((relsig / reltest) * 100, 0)))
  cat("\014")
  readline("Paste into column C, then press ENTER key")
  
  # Column D
  # Mean
  temp1 <- mean(sel$T_r..O., na.rm = TRUE)
  # SD
  temp2 <- sd(sel$T_r..O., na.rm = TRUE)
  
  writeClipboard(paste0(round(temp1, 2), " (", round(temp2, 2), ")"))
  cat("\014")
  readline("Paste into column D, then press ENTER key")
  
  # Column E
  # Mean
  temp1 <- mean(sel$T_r..R., na.rm = TRUE)
  # SD
  temp2 <- sd(sel$T_r..R., na.rm = TRUE)
  
  writeClipboard(paste0(round(temp1, 2), " (", round(temp2, 2), ")"))
  cat("\014")
  readline("Paste into column E, then press ENTER key")
}

#---------------

# P-value analyses
# Written by CHJ Hartgerink

#  McNemar test
tab <- table(MASTER$T_sign..O.[!is.na(MASTER$T_sign..O.) & !is.na(MASTER$T_sign..R.)],
             MASTER$T_sign..R.[!is.na(MASTER$T_sign..O.) & !is.na(MASTER$T_sign..R.)])

mcnemarchi <- (tab[1,2]-tab[2,1])^2/(tab[1,2]+tab[2,1])
mcnemarp <- pchisq(q = mcnemarchi, df = 1, lower.tail = FALSE)

# Dependent t-test p-values
t.test(x = MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
       y = MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
       paired = TRUE)

# Wilcoxon signed-rank test p-values
wilcox.test(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
            MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
            alternative="two.sided")

sd(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])
summary(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])
sd(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])
summary(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])

# Percent significant and nonsignificant for original studies
# Significant
sum(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.)] <= .05) / length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.)])
# Nonsignificant
sum(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.)] > .05) / length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.)])

# Percent significant and nonsignificant for replication studies
# Significant
sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.)] <= .05) / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.)])
# Nonsignificant
sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.)] > .05) / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.)])

# Deviation from uniformity in nonsignificant replication studies
FisherMethod(x = MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.)],
             id = 1,
             alpha = .05)

# Distribution p-values
pdf('pvalue distributions.pdf', onefile = TRUE, width = 11, height = 9.2)
#PDF
plot(density(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]),
     lty=1,
     frame.plot=F, 
     main="P-value distributions (PDF)",
     xlim=c(0,1),
     xaxs="i",
     yaxs="i",
     xlab="P-value",
     ylab = "Density",
     cex.axis=.6,
     cex.lab=.7,
     col = "grey")
lines(density(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))
legend(x=.4,y=4,legend=c(paste('Original p-values, k = ',
                               length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])),
                         paste('Replication p-values, k = ',
                               length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))),
       cex=1,
       lty=c(1,1), bty = 'n',
       col = c("grey","black"),box.lwd=0)

# CDF
plot(ecdf(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]),
     lty=1,
     frame.plot=F, 
     main="P-value distributions (CDF)",
     xlim=c(0,1),
     xaxs="i",
     yaxs="i",
     xlab="P-value",
     ylab = "Density",
     cex.axis=.6,
     cex.lab=.7,
     col = "grey")
lines(ecdf(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))
legend(x=.5,y=.3,legend=c(paste('Original p-values, k = ',
                                length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])),
                          paste('Replication p-values, k = ',
                                length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))),
       cex=1,
       lty=c(1,1), bty = 'n',
       col = c("grey","black"),box.lwd=0)
dev.off()

# Significance per journal

# Table significance, per journal
d11 <- sum(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & MASTER$Journal..O. == "JPSP"] <= .05)
d11perc <- d11 / length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & MASTER$Journal..O. == "JPSP"])
d12 <- sum(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & MASTER$Journal..O. == "JEPLMC"] <= .05)
d12perc <- d12 / length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & MASTER$Journal..O. == "JEPLMC"])
d13 <- sum(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & MASTER$Journal..O. == "PS"] <= .05)
d13perc <- d13 / length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & MASTER$Journal..O. == "PS"])

d21 <- sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JPSP"] <= .05)
d21perc <- d21 / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JPSP"])
d22 <- sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JEPLMC"] <= .05)
d22perc <- d22 / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JEPLMC"])
d23 <- sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "PS"] <= .05)
d23perc <- d23 / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "PS"])

d21cond <- sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JPSP" & 
                                       MASTER$T_pval_USE..O. <= .05] <= .05)
d21perccond <- d21 / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JPSP" & 
                                                    MASTER$T_pval_USE..O. <= .05])
d22cond <- sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JEPLMC" &
                                       MASTER$T_pval_USE..O. <= .05] <= .05)
d22perccond <- d22 / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "JEPLMC" &
                                                    MASTER$T_pval_USE..O. <= .05])
d23cond <- sum(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "PS" &
                                       MASTER$T_pval_USE..O. <= .05] <= .05)
d23perccond <- d23 / length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.) & MASTER$Journal..O. == "PS" &
                                                    MASTER$T_pval_USE..O. <= .05])

labels <- c("Original", "Replication", "Replication | sig original")

col1 <- c(d11, d21, d21cond)
col1perc <- c(d11perc, d21perc, d21perccond)
col2 <- c(d12, d22, d22cond)
col2perc <- c(d12perc, d22perc, d22perccond)
col3 <- c(d13, d23, d23cond)
col3perc <- c(d13perc, d23perc, d23perccond)

cbind(labels, col1, col1perc, col2, col2perc, col3, col3perc)

#---------------

# Effect size distributions
# Written by CHJ Hartgerink

# Original effects recomputed
length(MASTER$T_r..O.[!is.na(MASTER$T_r..O.)])
# Replication effects recopmuted
length(MASTER$T_r..R.[!is.na(MASTER$T_r..R.)])

# Number of pairs
sum(!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.))

# Dependent t-test effects (r values)
t.test(x = MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
       y = MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
       paired = TRUE)


# Wilcox test effects (r values)
wilcox.test(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
            MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
            alternative="two.sided")

summary(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])
sd(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])
summary(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])
sd(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])

mean(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])-mean(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])

# Binomial test to see if replicated effect is larger than original
temp <- MASTER$T_Comparison.effects..R.O.
prop <- sum(temp[!is.na(temp)])/length(temp[!is.na(temp)])
binom.test(x = sum(temp[!is.na(temp)]), n = length(temp[!is.na(temp)]),
           p = .5,alternative = "two.sided")

pdf('effect plots.pdf', width = 11.2, height = 9, onefile = TRUE)
# Effect replication and original
plot(y = MASTER$T_r..R., x = MASTER$T_r..O., xlab = "Effect size r (original)",
     ylab = "Effect size r (replication)", col = "white")
points(y = MASTER$T_r..R.[MASTER$T_pval_USE..O. > .05 & MASTER$T_pval_USE..R. > .05],
       x = MASTER$T_r..O.[MASTER$T_pval_USE..O. > .05 & MASTER$T_pval_USE..R. > .05], pch = 4)
points(y = MASTER$T_r..R.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. > .05],
       x = MASTER$T_r..O.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. > .05], pch = 21)
points(y = MASTER$T_r..R.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. <= .05],
       x = MASTER$T_r..O.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. <= .05], pch = 10)
lines(x = seq(0, 1, .001), y = seq(0, 1, .001), type = 'l')

# Regression line
x1 <- MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]
y1 <- MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]

# Linear
xx <- lm(y1 ~ x1)
lines(loess.smooth(x = x1, y = y1), lty = 2)
curve(expr = (xx$coefficients[1] + xx$coefficients[2] * x), from = -10, to = 10, add = TRUE, col = "blue")

legend(x=.6,y=.0,legend=c('Both nonsignificant',
                          'Original significant',
                          'Both significant',
                          'Repl. predicted by orig.',
                          'Loess curve'),
       cex=1,
       lty=c(0, 0, 0, 1, 2), bty = 'n', pch = c(4, 21, 10, NA, NA),
       col = c("black", "black", "black", "blue", "black"),box.lwd=0)

# R2
xr2 <- x1 * x1
yr2 <- y1 * y1

plot(y = yr2, x = xr2, xlab = "Effect size r (original)",
     ylab = "Effect size r (replication)", col = "white", ylim = c(0,1), xlim = c(0,1))
points(y = yr2[MASTER$T_pval_USE..O. > .05 & MASTER$T_pval_USE..R. > .05],
       x = xr2[MASTER$T_pval_USE..O. > .05 & MASTER$T_pval_USE..R. > .05], pch = 4)
points(y = yr2[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. > .05],
       x = xr2[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. > .05], pch = 21)
points(y = yr2[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. <= .05],
       x = xr2[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. <= .05], pch = 10)
lines(x = seq(0, 1, .001), y = seq(0, 1, .001), type = 'l')

r2 <- lm(yr2 ~ xr2)
lines(loess.smooth(x = xr2, y = yr2), lty = 2)
curve(expr = (r2$coefficients[1] + r2$coefficients[2] * x), from = 0, to = 1, add = TRUE, col = "blue")

legend(x=.6,y=.2,legend=c('Both nonsignificant',
                          'Original significant',
                          'Both significant',
                          'Repl. predicted by orig.',
                          'Loess curve'),
       cex=1,
       lty=c(0, 0, 0, 1, 2), bty = 'n', pch = c(4, 21, 10, NA, NA),
       col = c("black", "black", "black", "blue", "black"),box.lwd=0)
# Histogram effects 
hist1 <- hist(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)], breaks=15)
hist2 <- hist(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)], breaks=20)
plot(hist1, xlim = c(-.6, 1.2), ylim = c(0, 50), xlab = "Effect size r",
     main = "Effect histograms")
plot(hist2, add = TRUE, col = addTrans("grey",150))
legend(x = .7, y = 25,
       leg=c(paste("Original, k = ",
                   length(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])),
             paste("Replication, k = ",
                   length(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]))),
       fill=c("white", "grey"),
       bty = 'n')

# CDF effects
plot(ecdf(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]),
     lty=1,
     frame.plot=F, 
     main="Cumulative effect distributions",
     xlim=c(0,1),
     xaxs="i",
     yaxs="i",
     xlab="Correlation coefficient",
     ylab = "Cumulative density",
     cex.axis=.6,
     cex.lab=.7,
     col = "grey")
lines(ecdf(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]))
legend(x=.6,y=.5,legend=c(paste("Original, k = ",
                                length(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])),
                          paste("Replication, k = ",
                                length(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]))),
       cex=1,
       lty=c(1,1), bty = 'n',
       col = c("grey","black"),box.lwd=0)

dev.off()

#----------------------

# Meta-analysis
# Written by RCM van Aert

##########################################
### Create correct order of categories ###
##########################################

### Create the right order of the variable "Perceived expertise required"
fac.expe <- factor(MASTER$Methodology.expertise.required..O., levels = c("No expertise required", "Slight expertise required",
                                                                         "Moderate expertise required", "Strong expertise required", 
                                                                         "Extreme expertise required"))

### Create the right order of the variable "Opportunity for expectancy biases"
fac.oppo.expe <- factor(MASTER$Opportunity.for.expectancy.bias..O., levels = c("No opportunity for researcher expectations to influence results",
                                                                               "Slight opportunity for researcher expectations to influence results",
                                                                               "Moderate opportunity for researcher expectations to influence results",
                                                                               "Strong opportunity for researcher expectations to influence results",
                                                                               "Extreme opportunity for researcher expectations to influence results"))

### Create the right order of the variable "Opportunity for lack of diligence"
fac.oppo.dili <- factor(MASTER$Opportunity.for.lack.of.diligence..O., levels = c("No opportunity for lack of diligence to affect the results",
                                                                                 "Slight opportunity for lack of diligence to affect the results",
                                                                                 "Moderate opportunity for lack of diligence to affect the results",
                                                                                 "Strong opportunity for lack of diligence to affect the results",
                                                                                 "Extreme opportunity for lack of diligence to affect the results"))

### Create the right order of the variable "Current position"
fac.posi <- factor(MASTER$Current.position..R., levels = c("Professor (or equivalent)", "Associate Professor (or equivalent)", "Assistant Professor (or equivalent)",
                                                           "Post-doc or Research Scientist", "PhD student", "Master's student", "Undergraduate student", 
                                                           "Faculty (non-tenure track) full-time lecturer", "Private sector researcher"))

### Change "Faculty (non-tenure track) full-time lecturer" into "Assistant Professor (or equivalent)"
tmp <- numeric()
for(i in 1:length(fac.posi)) {
  if(is.na(fac.posi[i] == TRUE)) { tmp[i] <- NA }
  else if(as.character(fac.posi[i]) == "Faculty (non-tenure track) full-time lecturer") { tmp[i] <- "Assistant Professor (or equivalent)"
  } else if (as.character(fac.posi[i]) == "Private sector researcher") { tmp[i] <- "Post-doc or Research Scientist"
  } else { tmp[i] <- as.character(fac.posi[i]) }
}
fac.posi <- as.factor(tmp)

### Create the right order of the variable "Degree"
fac.degr <- factor(MASTER$Degree..R., levels = c("PhD or equivalent", "Master's degree or equivalent", "Some graduate school", "Bachelor's degree or equivalent", "some college/university",
                                                 "high school/equivalent"))

### Create the right order of the variable "Domain expertise"
fac.doma <- factor(MASTER$Domain.expertise..R., levels = c("No Expertise", "Slight Expertise", "Some Expertise", "Moderate Expertise", "High Expertise"))

### Create the right order of the variable "Method expertise"
fac.meth <- factor(MASTER$Method.expertise..R., levels = c("No Expertise", "Slight Expertise", "Some Expertise", "Moderate Expertise", "High Expertise"))

### Create the right order of the variable "Implementation quality"
fac.impl <- factor(MASTER$Implementation.quality..R., levels = c("was of much higher quality than the original study", "was of moderately higher quality than the original study",
                                                                 "was of slightly higher quality than the original study", "was about the same quality as the original study", 
                                                                 "was of slightly lower quality than the original study", "was of moderately lower quality than the original study"))

### Create the right order of the variable "Data collection quality"
fac.data <- factor(MASTER$Data.collection.quality..R., levels = c("was much better than the average study", "was better than the average study", "was slightly better than the average study",
                                                                  "was about the same as the average study", "was slightly worse than the average study", "was worse than the average study",
                                                                  "was much worse than the average study"))

### Create the right order of the variable "Replication similarity"
fac.repl <- factor(MASTER$Replication.similarity..R., levels = c("Not at all similar", "Slightly similar", "Somewhat similar", "Moderately similar", "Very similar", "Extremely similar",
                                                                 "Virtually identical"))

### Create the right order of the variable "Difficulty of implementation"
fac.diff <- factor(MASTER$Difficulty.of.implimentation..R., levels = c("Extremely challenging", "Very challenging", "Moderately challenging", "Somewhat challenging", "Slightly challenging",
                                                                       "Not at all challenging"))

###########################################
### Prepare variables for meta-analyses ###
###########################################

### Function for standardizing variables
stand <- function(x, max, min, option) {
  
  if(option == 1) {
    res <- (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  }
  
  if(option == 2) {
    res <- (x - min)/(max-min)
  }
  
  return(res)
}

### OPTION 1 FOR STANDARDIZING ###
option <- 1

# a. PUBLISHING JOURNAL AND SUBDISCIPLINE

### Create dummy variables for "Publishing journal and subdiscipline"
### JPSP is reference category because it has the most cases
d.JEP <- ifelse(jour == 1, 1, 0)
d.PSCog <- ifelse(jour == 3, 1, 0)
d.PSSoc <- ifelse(jour == 4, 1, 0)
d.PSOth <- ifelse(jour == 5, 1, 0)

# b. IMPORTANCE OF THE EFFECT

### Standardizing "Citation count, paper (O)"
st.impa <- stand(MASTER$Citation.count..paper..O., option = option)

### Standardizing "Exciting/important effect"
st.exci <- stand(as.numeric(levels(MASTER$Exciting.result..O.))[MASTER$Exciting.result..O.], option = option)

### Creating scale
sc.impo <- (st.impa + st.exci)/2

# c. SURPRISING EFFECT

### Standardizing "Surprising effect" and creating scale
sc.surp <- stand(as.numeric(levels(MASTER$Surprising.result..O.))[MASTER$Surprising.result..O.], option = option)

# d. EXPERIENCE AND EXPERTISE OF ORIGINAL TEAM

### Taking the average and then standardizing "Institution prestige of 1st author and senior author" 
ave.pres <- (MASTER$Institution.prestige..1st.author..O.+MASTER$Institution.prestige..senior.author..O.)/2
st.pres <- stand(ave.pres, option = option)

### Standardizing "Citation Count, 1st author (O)"
st.impa.1st <- stand(MASTER$Citation.Count..1st.author..O., option = option)

### Standardizing "Citation count, senior author (O)"
st.impa.sen <- stand(MASTER$Citation.count..senior.author..O., option = option)

### Creating scale
sc.expe1 <- (st.pres + st.impa.1st + st.impa.sen)/3

# e. CHALLENGE OF CONDUCTING REPLICATION

### Standardizing "Perceived expertise required"
st.expe <- stand(as.numeric(fac.expe), option = option)

### Standardizing "Perceived opportunity for expectancy biases"
st.oppo.expe <- stand(as.numeric(fac.oppo.expe), option = option)

### Standardizing "Perceived opportunity for impact of lack of diligence"
st.oppo.dili <- stand(as.numeric(fac.oppo.dili), option = option)

### Create scale
sc.chal <- (st.expe + st.oppo.expe + st.oppo.dili)/3

# f. EXPERIENCE AND EXPERTISE OF REPLICATION TEAM

### Standardizing "Position of senior member of replication team"
st.posi <- stand(as.numeric(fac.posi), option = option)

### Standardizing "Highest degree of replication team's senior member"
st.degr <- stand(as.numeric(fac.degr), option = option)

### Standardizing "Replication team domain expertise"
st.doma <- stand(as.numeric(fac.doma), option = option)

### Standardizing "Replication team method expertise"
st.meth <- stand(as.numeric(fac.meth), option = option)

### Standardizing "Replication team senior member's total publications"
st.publ <- stand(MASTER$Total.publications..R., option = option)

### Standardizing "Replication team senior member's total publications and total number of peer-reviewed articles"
st.peer <- stand(MASTER$Peer.reviewed.articles..R., option = option)

### Standardizing "Replication team senior member's total citations"
st.cita <- stand(MASTER$Citations..R., option = option)

### Create scale
sc.expe2 <- (st.posi + st.degr + st.doma + st.meth + st.publ + st.cita)/6

# g. SELF-ASSESSED QUALITY OF REPLICATION

### Standardizing "Self-assessed quality of replication"
st.impl <- stand(as.numeric(fac.impl), option = option)

### Standardizing "Self-assessed data collection quality of replication"
st.data <- stand(as.numeric(fac.data), option = option)

### Standardizing "Self-assessed replication similarity to original"
st.repl <- stand(as.numeric(fac.repl), option = option)

### Standardizing "Self-assessed difficulty of implementation"
st.diff <- stand(as.numeric(fac.diff), option = option)

### Create a scale
sc.self <- (st.impl + st.data + st.repl + st.diff)/4

###########################################
### Transforming correlations to Fisher ###
###########################################

ri.o <- MASTER$T_r..O.
ri.r <- MASTER$T_r..R.
N.o <- MASTER$T_df2..O.+2
N.r <- MASTER$T_df2..R.+2

### Transform to Fisher's z
fis.o <- 0.5*log((1 + ri.o) / (1 - ri.o)) 
fis.r <- 0.5*log((1 + ri.r) / (1 - ri.r))

### Difference in Fisher's z scores
yi <- numeric()
for(i in 1:length(fis.o)) {
  
  if(is.na(fis.o[i]) == TRUE | is.na(fis.r[i]) == TRUE) { yi[i] <- NA }
  else if(fis.o[i] < 0 & fis.r[i] < 0) { yi[i] <- fis.o[i]*-1-fis.r[i]*-1 } 
  else if(fis.o[i] < 0 & fis.r[i] > 0) { yi[i] <- fis.o[i]*-1+fis.r[i] }
  else {  yi[i] <- fis.o[i]-fis.r[i] }
  
}

### Standard errors original and replication study
sei.o <- sqrt(1/(N.o-3))
sei.r <- sqrt(1/(N.r-3))

### p-values original and replication study
pval.o <- pnorm(fis.o, sd = sei.o, lower.tail = FALSE)
pval.r <- pnorm(fis.r, sd = sei.r, lower.tail = FALSE)

### Standard error of difference score
sei <- sqrt(1/(N.o-3) + 1/(N.r-3))

#######################################
### Select studies for the analyses ###
#######################################

df <- data.frame(ID = MASTER$ID, stat = as.character(MASTER$T_Test.Statistic..O.), df1 = MASTER$T_df1..O., yi, sei, fis.o, sei.o, N.o, pval.o, fis.r, sei.r, N.r, pval.r, 
                 d.JEP, d.PSCog, d.PSSoc, d.PSOth, sc.impo, sc.surp, sc.expe1, sc.chal, sc.expe2, sc.self, power.r = as.numeric(levels(MASTER$Power..R.))[MASTER$Power..R.])
df[149, ] <- NA ### Remove duplicate

### Select: F(df1 = 1, df2), t, and r
sub <- subset(df, (df$stat == "F" & df$df1 == 1) | df$stat == "t" | df$stat == "r")

### Remove rows when NA on yi
final <- sub[!is.na(sub$yi) & !is.na(sub$sei), ]

####################
### Tables Brian ###
####################

### Load metafor package
library(metafor)

### Meta-analytic mean and sd of the estimate combining per pair the original study and replication 
est <- pval <- numeric()

for(i in 1:length(final$fis.o)) {
  tmp <- rma(yi = c(final$fis.o[i], final$fis.r[i]), sei = c(final$sei.o[i], final$sei.r[i]), method = "FE")
  est[i] <- tmp$b[1]
  if (tmp$pval < 0.05) { pval[i] <- TRUE
  } else { pval[i] <- FALSE }
}

### Transforming Fisher z values into raw correlations
cor <- (exp(2*est)-1)/(exp(2*est)+1)

mean(cor)
sd(cor)
sum(pval)/length(pval) # Proportion of statistically significant studies

### Per discipline
mean(cor[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0 & final$d.PSOth == 0])
mean(cor[final$d.JEP == 1])
mean(cor[final$d.PSSoc == 1])
mean(cor[final$d.PSCog == 1])
mean(cor[final$d.PSOth == 1])

sd(cor[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0 & final$d.PSOth == 0])
sd(cor[final$d.JEP == 1])
sd(cor[final$d.PSSoc == 1])
sd(cor[final$d.PSCog == 1])
sd(cor[final$d.PSOth == 1])

sum(pval[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0 & final$d.PSOth == 0])/length(pval[final$d.JEP == 0 
                                                                                                       & final$d.PSSoc == 0 & final$d.PSCog == 0 & final$d.PSOth == 0])
sum(pval[final$d.JEP == 1])/length(pval[final$d.JEP == 1])
sum(pval[final$d.PSSoc == 1])/length(pval[final$d.PSSoc == 1])
sum(pval[final$d.PSCog == 1])/length(pval[final$d.PSCog == 1])
sum(pval[final$d.PSOth == 1])/length(pval[final$d.PSOth == 1])

### Subset of data with all cases which were replicated
tmp <- subset(df, is.na(df$fis.r) == FALSE)
summary(tmp$N.o)
summary(tmp$N.r)
mean(tmp$power.r, na.rm = TRUE)

### Subset of data with all cases published in JPSP
tmp.JPSP <- subset(tmp, tmp$d.JEP == 0 & tmp$d.PSSoc == 0 & tmp$d.PSCog == 0 & tmp$d.PSOth == 0)
summary(tmp.JPSP$N.o)
summary(tmp.JPSP$N.r)
mean(tmp.JPSP$power.r, na.rm = TRUE)

### Subset of data with all cases published in JEP
tmp.JEP <- subset(tmp, tmp$d.JEP == 1)
summary(tmp.JEP$N.o)
summary(tmp.JEP$N.r)
mean(tmp.JEP$power.r, na.rm = TRUE)

### Subset of data with all cases published in PSSoc
tmp.PSSoc <- subset(tmp, tmp$d.PSSoc == 1)
summary(tmp.PSSoc$N.o)
summary(tmp.PSSoc$N.r)
mean(tmp.PSSoc$power.r, na.rm = TRUE)

### Subset of data with all cases published in PSCog
tmp.PSCog <- subset(tmp, tmp$d.PSCog == 1)
summary(tmp.PSCog$N.o)
summary(tmp.PSCog$N.r)
mean(tmp.PSCog$power.r, na.rm = TRUE)

### Subset of data with all cases published in PSOth
tmp.PSOth <- subset(tmp, tmp$d.PSOth == 1)
summary(tmp.PSOth$N.o)
summary(tmp.PSOth$N.r)
mean(tmp.PSOth$power.r, na.rm = TRUE)

### How often is original study within CI of replication
### Create confidence interval for replications
ci.lb <- final$fis.r-qnorm(.975)*final$sei.r
ci.ub <- final$fis.r+qnorm(.975)*final$sei.r

in.ci <- numeric()

for(i in 1:length(final$fis.r)) {
  
  if (final$fis.o[i] > ci.lb[i] & final$fis.o[i] < ci.ub[i]) {
    in.ci[i] <- TRUE
  } else { in.ci[i] <- FALSE }
  
}

sum(in.ci)/length(in.ci) # Proportion of times the original study is within the CI of the replication

### Per discipline
sum(in.ci[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0 & final$d.PSOth == 0])/length(in.ci[final$d.JEP == 0 
                                                                                                         & final$d.PSSoc == 0 & final$d.PSCog == 0 & final$d.PSOth == 0])
sum(in.ci[final$d.JEP == 1])/length(in.ci[final$d.JEP == 1])
sum(in.ci[final$d.PSSoc == 1])/length(in.ci[final$d.PSSoc == 1])
sum(in.ci[final$d.PSCog == 1])/length(in.ci[final$d.PSCog == 1])
sum(in.ci[final$d.PSOth == 1])/length(in.ci[final$d.PSOth == 1])

### Predicting how often effect size of original study is within CI of replication
overlap <- numeric()
points <- 1000000
p <- 1:points/(points+1)

for (i in 1:length(final$N.r)) {
  zu <- qnorm(p,0,1/sqrt(final$N.r[i]-3)) + qnorm(.975)/sqrt(final$N.r[i]-3)
  zl <- zu - 2*qnorm(.975)/sqrt(final$N.r[i]-3)
  overlap[i] <- mean(pnorm(zu,0,1/sqrt(final$N.o[i]-3))) - mean(pnorm(zl,0,1/sqrt(final$N.o[i]-3)))
}
overlap
mean(overlap)

#################################################################################
### Meta-analyses based on differences between original study and replication ###
#################################################################################

### Meta-analysis of null model
res <- rma(yi = final$yi, sei = final$sei, method = "REML")
res
# png("C:/Users/S787802/Desktop/Funnel RPP.png", width = 900, height = 900, res = 200, pointsize = 5)
funnel(res, main = "Funnel plot based on difference original and replication study")
# dev.off()

### Meta-analysis with a. PUBLISHING JOURNAL AND SUBDISCIPLINE as moderator (JPSP is reference category)
rma(yi = final$yi, sei = final$sei, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "REML")

### Meta-analysis with sei.o as moderator
rma(yi = final$yi, sei = final$sei, mods = ~ final$sei.o, method = "REML")

### Meta-analysis with a. PUBLISHING JOURNAL AND SUBDISCIPLINE and sei.o as moderators (JPSP is reference category)
rma(yi = final$yi, sei = final$sei, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth + final$sei.o, method = "REML")

### Meta-analysis with b. IMPORTANCE OF THE EFFECT as moderator
rma(yi = final$yi, sei = final$sei, mods = ~ final$sc.impo, method = "REML")

### Meta-analysis with c. SURPRISING EFFECT as moderator
rma(yi = final$yi, sei = final$sei, mods = ~ final$sc.surp, method = "REML")

### Meta-analysis with d. EXPERIENCE AND EXPERTISE OF ORIGINAL TEAM as moderator
rma(yi = final$yi, sei = final$sei, mods = ~ final$sc.expe1, method = "REML")

### Meta-analysis with e. CHALLENGE OF CONDUCTING REPLICATION as moderator
rma(yi = final$yi, sei = final$sei, mods = ~ final$sc.chal, method = "REML")

### Meta-analysis with f. EXPERIENCE AND EXPERTISE OF REPLICATION TEAM
rma(yi = final$yi, sei = final$sei, mods = ~ final$sc.expe2, method = "REML")

### Meta-analysis with g. SELF-ASSESSED QUALITY OF REPLICATION
rma(yi = final$yi, sei = final$sei, mods = ~ final$sc.self, method = "REML")

####################################################
### Meta-analyses based on only original studies ###
####################################################

### Meta-analysis of null model
res <- rma(yi = final$fis.o, sei = final$sei.o, method = "REML")
res
# png("C:/Users/S787802/Desktop/Funnel original RPP.png", width = 900, height = 900, res = 200, pointsize = 5)
funnel(res, main = "Funnel plot based on original studies")
# dev.off()

### Meta-analysis with se in original study as moderator
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o, method = "REML")

### Meta-analysis with se in original study as moderator with "ML" as estimator for tau2 (model comparison)
res0 <- rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o, method = "ML")
res0

### Meta-analysis with se in original study and dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "REML")

### Meta-analysis with se in original study and dummy variables for journal and discipline (JPSP is reference category) with "ML" as estimator for tau2 (model comparison)
res1 <- rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "ML")
res1

### Comparing models with and without discipline
anova(res0, res1)

### Meta-analysis with dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "REML")

### Meta-analysis with se in original study, dummy variables for journal and discipline (JPSP is reference category), and its interactions as moderators
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth + final$sei.o*final$d.JEP + final$sei.o*final$d.PSSoc + final$sei.o*final$d.PSOth, method = "REML")

#######################################################
### Meta-analyses based on only replication studies ###
#######################################################

### Meta-analysis of null model
res <- rma(yi = final$fis.r, sei = final$sei.r, method = "REML")
res
# png("C:/Users/S787802/Desktop/Funnel replication RPP.png", width = 900, height = 900, res = 200, pointsize = 5)
funnel(res, main = "Funnel plot based on replication studies")
# dev.off()

### Meta-analysis with se in replication study as moderator
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r, method = "REML")

### Meta-analysis with se in replication study as moderator with "ML" as estimator for tau2 (model comparison)
res0 <- rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r, method = "ML")
res0

### Meta-analysis with se in replication study and dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "REML")

### Meta-analysis with se in replication study and dummy variables for journal and discipline (JPSP is reference category) with "ML" as estimator for tau2 (model comparison)
res1 <- rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "ML")
res1

### Comparing models with and without discipline
anova(res0, res1)

### Meta-analysis with se in replication study and dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "REML")

### Meta-analysis with dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth, method = "REML")

### Meta-analysis with se in replication study, dummy variables for journal and discipline (JPSP is reference category), and its interactions as moderators
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$d.PSOth + final$sei.r*final$d.JEP + final$sei.r*final$d.PSSoc + final$sei.r*final$d.PSOth, method = "REML")

##############################
### Meta-analyses per pair ###
##############################

### How often is the null hypotheses rejected in the meta-analysis
in.ci <- es.meta <- se.meta <- ci.lb.meta <- ci.ub.meta <- pval.meta <- numeric()

for(i in 1:length(final$fis.o)) {
  tmp <- rma(yi = c(final$fis.o[i], final$fis.r[i]), sei = c(final$sei.o[i], final$sei.r[i]), method = "FE")
  es.meta[i] <- tmp$b[1]
  se.meta[i] <- tmp$se
  ci.lb.meta[i] <- tmp$ci.lb
  ci.ub.meta[i] <- tmp$ci.ub
  pval.meta[i] <- tmp$pval
  
  if(tmp$pval < 0.05) { in.ci[i] <- 1
  } else { in.ci[i] <- 0 }
}

sum(in.ci)/length(in.ci) # Proportion of times the null hypothesis of no effect is rejected

### Create data frame
tab <- data.frame(ID = final$ID, fis.o = final$fis.o, sei.o = final$sei.o, pval.o = final$pval.o, fis.r = final$fis.r, sei.r = final$sei.r, 
                  pval.r = final$pval.r, diff = final$yi, es.meta = es.meta, se.meta = se.meta, ci.lb.meta = ci.lb.meta, ci.ub.meta = ci.ub.meta, pval.meta = pval.meta)

### Check how often effect size original study is within CI of meta-analysis
in.ci.meta <- numeric()

for(i in 1:length(final$fis.o)) {
  
  if(final$fis.o[i] > ci.lb.meta[i] & final$fis.o[i] < ci.ub.meta[i]) {
    in.ci.meta[i] <- TRUE
  } else { in.ci.meta[i] <- FALSE }
  
}

sum(in.ci.meta)/length(in.ci.meta) # Proportion of times the original study is within the CI of meta-analysis

############################################################
### How often is original study within CI of replication ###
############################################################

### Create confidence interval for replications
ci.lb <- final$fis.r-qnorm(.975)*final$sei.r
ci.ub <- final$fis.r+qnorm(.975)*final$sei.r

in.ci <- numeric()

for(i in 1:length(final$fis.r)) {
  
  if (final$fis.o[i] > ci.lb[i] & final$fis.o[i] < ci.ub[i]) {
    in.ci[i] <- TRUE
  } else { in.ci[i] <- FALSE }
  
}

sum(in.ci)/length(in.ci) # Proportion of times the original study is within the CI of the replication

### Predicting how often effect size of original study is within CI of replication
overlap <- numeric()
points <- 1000000
p <- 1:points/(points+1)

for (i in 1:length(final$N.r)) {
  zu <- qnorm(p,0,1/sqrt(final$N.r[i]-3)) + qnorm(.975)/sqrt(final$N.r[i]-3)
  zl <- zu - 2*qnorm(.975)/sqrt(final$N.r[i]-3)
  overlap[i] <- mean(pnorm(zu,0,1/sqrt(final$N.o[i]-3))) - mean(pnorm(zl,0,1/sqrt(final$N.o[i]-3)))
}
overlap
mean(overlap)

#######################################
### Correlations between moderators ###
#######################################

### Load package for correlation table
library(Hmisc)

### Create variable with meta-analysis estimate with NAs
es.meta.all <- vector(length = nrow(df))
s <- 1
for(i in 1:max(final$ID)) {
  if (i == final$ID[s]) {
    es.meta.all[i] <- es.meta[s]
    s <- s + 1
  } else {
    es.meta.all[i] <- NA
  }
}

### Create matrix for correlation table
mat <- cbind(df$fis.o, df$fis.r, df$yi, es.meta.all, df$sc.imp, df$sc.surp, df$sc.expe1, df$sc.chal, df$sc.expe2, df$sc.self)
colnames(mat) <- c("fis.o", "fis.r", "diff", "es.meta", "importance", "surprising", "experience.O", "challenge", "experience.R", "quality")

### Subset of only the replicated studies
mat <- subset(mat, is.na(df$fis.r) == FALSE)

### Create table with correlations
rcorr(mat, type = "pearson")

### Option 2 for standardizing moderators
option <- 2

# b. IMPORTANCE OF THE EFFECT

### Standardizing "Citation count, paper (O)"
st.impa <- stand(MASTER$Citation.count..paper..O., max = max(MASTER$Citation.count..paper..O., na.rm = TRUE), min = min(MASTER$Citation.count..paper..O., na.rm = TRUE), option = option)

### Standardizing "Exciting/important effect"
st.exci <- stand(as.numeric(levels(MASTER$Exciting.result..O.))[MASTER$Exciting.result..O.], max = 6, min = 1, option = option)

### Creating scale
sc.impo <- (st.impa + st.exci)/2

# c. SURPRISING EFFECT

### Standardizing "Surprising effect" and creating scale
sc.surp <- stand(as.numeric(levels(MASTER$Surprising.result..O.))[MASTER$Surprising.result..O.], max = 6, min = 1, option = option)

# d. EXPERIENCE AND EXPERTISE OF ORIGINAL TEAM

### Taking the average and then standardizing "Institution prestige of 1st author and senior author" 
ave.pres <- (MASTER$Institution.prestige..1st.author..O.+MASTER$Institution.prestige..senior.author..O.)/2
st.pres <- stand(ave.pres, min = 2, max = 10, option = option)

### Standardizing "Citation Count, 1st author (O)"
st.impa.1st <- stand(MASTER$Citation.Count..1st.author..O., max = max(MASTER$Citation.Count..1st.author..O., na.rm = TRUE), min = min(MASTER$Citation.Count..1st.author..O., na.rm = TRUE), option = option)

### Standardizing "Citation count, senior author (O)"
st.impa.sen <- stand(MASTER$Citation.count..senior.author..O., max = max(MASTER$Citation.count..senior.author..O., na.rm = TRUE), min = min(MASTER$Citation.count..senior.author..O., na.rm = TRUE), option = option)

### Creating scale
sc.expe1 <- (st.pres + st.impa.1st + st.impa.sen)/3

# e. CHALLENGE OF CONDUCTING REPLICATION

### Standardizing "Perceived expertise required"
st.expe <- stand(as.numeric(fac.expe), max = 1, min = 5, option = option)

### Standardizing "Perceived opportunity for expectancy biases"
st.oppo.expe <- stand(as.numeric(fac.oppo.expe), max = 4, min = 1, option = option)

### Standardizing "Perceived opportunity for impact of lack of diligence"
st.oppo.dili <- stand(as.numeric(fac.oppo.dili), max = 4, min = 1, option = option)

### Create scale
sc.chal <- (st.expe + st.oppo.expe + st.oppo.dili)/3

# f. EXPERIENCE AND EXPERTISE OF REPLICATION TEAM

### Standardizing "Position of senior member of replication team"
st.posi <- stand(as.numeric(fac.posi), max = 7, min = 1, option = option)

### Standardizing "Highest degree of replication team's senior member"
st.degr <- stand(as.numeric(fac.degr), max = 6, min = 1, option = option)

### Standardizing "Replication team domain expertise"
st.doma <- stand(as.numeric(fac.doma), max = 5, min = 1, option = option)

### Standardizing "Replication team method expertise"
st.meth <- stand(as.numeric(fac.meth), max = 5, min = 1, option = option)

### Standardizing "Replication team senior member's total publications"
st.publ <- stand(MASTER$Total.publications..R., max = max(MASTER$Total.publications..R., na.rm = TRUE), min = min(MASTER$Total.publications..R., na.rm = TRUE), option = option)

### Standardizing "Replication team senior member's total publications and total number of peer-reviewed articles"
st.peer <- stand(MASTER$Peer.reviewed.articles..R., max = max(MASTER$Peer.reviewed.articles..R., na.rm = TRUE), min = min(MASTER$Peer.reviewed.articles..R., na.rm = TRUE), option = option)

### Standardizing "Replication team senior member's total citations"
st.cita <- stand(MASTER$Citations..R., max = max(MASTER$Citations..R., na.rm = TRUE), min = min(MASTER$Citations..R., na.rm = TRUE), option = option)

### Create scale
sc.expe2 <- (st.posi + st.degr + st.doma + st.meth + st.publ + st.cita)/6

# g. SELF-ASSESSED QUALITY OF REPLICATION

### Standardizing "Self-assessed quality of replication"
st.impl <- stand(as.numeric(fac.impl), max = 6, min = 1, option = option)

### Standardizing "Self-assessed data collection quality of replication"
st.data <- stand(as.numeric(fac.data), max = 7, min = 1, option = option)

### Standardizing "Self-assessed replication similarity to original"
st.repl <- stand(as.numeric(fac.repl), max = 7, min = 1, option = option)

### Standardizing "Self-assessed difficulty of implementation"
st.diff <- stand(as.numeric(fac.diff), max = 6, min = 1, option = option)

### Create a scale
sc.self <- (st.impl + st.data + st.repl + st.diff)/4

### Select the same studies as with the other option for standardizing
df <- data.frame(ID = MASTER$ID, stat = as.character(MASTER$T_Test.Statistic..O.), df1 = MASTER$T_df1..O., yi, sei, fis.o, sei.o, N.o, pval.o, fis.r, sei.r, N.r, pval.r, 
                 d.JEP, d.PSCog, d.PSSoc, d.PSOth, sc.impo, sc.surp, sc.expe1, sc.chal, sc.expe2, sc.self)
df[149, ] <- NA ### Remove duplicate

### Create matrix for correlation table
mat <- cbind(df$fis.o, df$fis.r, df$yi, es.meta.all, df$sc.imp, df$sc.surp, df$sc.expe1, df$sc.chal, df$sc.expe2, df$sc.self)
colnames(mat) <- c("fis.o", "fis.r", "diff", "es.meta", "importance", "surprising", "experience.O", "challenge", "experience.R", "quality")

### Subset of only the replicated studies
mat <- subset(mat, is.na(df$fis.r) == FALSE)

### Create table with correlations
rcorr(mat, type = "pearson")