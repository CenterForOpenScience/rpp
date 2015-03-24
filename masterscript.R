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
# ALL ANALYSES INCLUDE PAIRWISE SELECTION.

#-----------------------------

# Writing out data for table
# https://docs.google.com/spreadsheets/d/16aqIekNerZcflSJwM7dOiXnN24-KvZqaa3eTg4js41U/edit#gid=0
# Columns B-E
### Recode "Publishing journal and subdiscipline"
jour <- numeric()

for(i in 1:nrow(MASTER)) {
  if(as.character(MASTER$Journal..O.[i]) == "JEPLMC") {
    jour[i] <- 2
  } else if(as.character(MASTER$Journal..O.[i]) == "JPSP") {
    jour[i] <- 1
  } else if(as.character(MASTER$Journal..O.[i]) == "PS") {
    if(as.character(MASTER$Discipline..O.[i]) == "Cognitive") {
      jour[i] <- 4
    } else if(as.character(MASTER$Discipline..O.[i]) == "Social") {
      jour[i] <- 3
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
for(journal in sort(unique(jour))){
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
plot(x = MASTER$T_r..R., y = MASTER$T_r..O., xlab = "Effect size r (replication)",
     ylab = "Effect size r (original)", col = "white")
points(x = MASTER$T_r..R.[MASTER$T_pval_USE..O. > .05 & MASTER$T_pval_USE..R. > .05],
       y = MASTER$T_r..O.[MASTER$T_pval_USE..O. > .05 & MASTER$T_pval_USE..R. > .05], pch = 4)
points(x = MASTER$T_r..R.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. > .05],
       y = MASTER$T_r..O.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. > .05], pch = 21)
points(x = MASTER$T_r..R.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. <= .05],
       y = MASTER$T_r..O.[MASTER$T_pval_USE..O. <= .05 & MASTER$T_pval_USE..R. <= .05], pch = 10)
lines(x = seq(0, 1, .001), y = seq(0, 1, .001), type = 'l')

# Regression line
xx <- lm(MASTER$T_r..R. ~ MASTER$T_r..O.)
curve(expr = (xx$coefficients[1] + xx$coefficients[2] * x), from = -10, to = 10, add = TRUE, col = "blue")

legend(x=.6,y=.25,legend=c('Both nonsignificant', 'Original significant', 'Both significant', 'Replication predicted by original'),
       cex=1,
       lty=c(0,0, 0, 1), bty = 'n', pch = c(4, 21, 10, NA),
       col = c("black", "black", "black", "blue"),box.lwd=0)

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

MASTER$T_r..R.^2 ~ MASTER$T_r..O.^2
dev.off()

###

# Meta-analysis
# Written by RCM van Aert

###

# Moderator analysis
# Written by RCM van Aert
