######################################################################################
# Master script Tilburg University Analyses                                          #
# Responsible for this file: CHJ Hartgerink (c.h.j.hartgerink@tilburguniversity.edu) # 
#                            RCM van Aert (r.c.m.vanaert@tilburguniversity.edu)      #
#                            MALM van Assen (m.a.l.m.vanassen@tilburguniversity.edu) #
######################################################################################

setwd(choose.dir())

# If you are having problems with reading in the data run this
# Thanks @hcp4715
# Sys.setlocale("LC_ALL", "English")

# source functions
if(!require(httr)){install.packages('httr')}
library(httr)
info <- GET('https://osf.io/b2vn7/?action=download', write_disk('functions.r', overwrite = TRUE)) #downloads data file from the OSF
source('functions.r')
if(!require(Hmisc)){install.packages('Hmisc')}
library(Hmisc)
if(!require(metafor)){install.packages('metafor')}
library(metafor)

# Read in Tilburg data
info <- GET('https://osf.io/fgjvw/?action=download', write_disk('rpp_data.csv', overwrite = TRUE)) #downloads data file from the OSF
MASTER <- read.csv("rpp_data.csv")[1:167, ]
colnames(MASTER)[1] <- "ID" # Change first column name to ID to be able to load .csv file

# ALL ANALYSES INCLUDE PAIRWISE SELECTION.

#-----------------------------
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
#-----------------------------

########################
# Preliminary analyses #
########################
# Written by CHJ Hartgerink

#  Table S1
tab <- table(MASTER$T_sign_O[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R)],
             MASTER$T_sign_R[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R)])
tab

#######################################################################
# Evaluating replication effect against null hypothesis of no effect. #
#######################################################################
# Written by CHJ Hartgerink

# Percent significant for original studies
sum(tab[2,])/sum(tab)
# Percent significant for replication studies
sum(tab[,2])/sum(tab)

mcnemarchi <- (tab[1,2]-tab[2,1])^2/(tab[1,2]+tab[2,1])
mcnemarp <- pchisq(q = mcnemarchi, df = 1, lower.tail = FALSE)
mcnemarchi; mcnemarp

# Significance per journal
# Table significance, per journal
# JEPLMC
JEPLMCtab <- table(MASTER$T_sign_O[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & jour == 1],
                   MASTER$T_sign_R[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & jour == 1])
# JPSP
JPSPtab <- table(MASTER$T_sign_O[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & jour == 2],
                 MASTER$T_sign_R[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & jour == 2])
# PS
PStab <- table(MASTER$T_sign_O[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & (jour == 3 | jour == 4 | jour == 5)],
               MASTER$T_sign_R[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & (jour == 3 | jour == 4 | jour == 5)])

labels <- c("Original", "Replication", "Replication | sig original")

JPSP <- c(sum(JPSPtab[2,]),
          sum(JPSPtab[,2]),
          sum(JPSPtab[2,2]))
JPSPperc <- c(sum(JPSPtab[2,])/sum(JPSPtab),
              sum(JPSPtab[,2])/sum(JPSPtab),
              sum(JPSPtab[2,2])/sum(JPSPtab[2,]))

JEPLMC <- c(sum(JEPLMCtab[2,]),
            sum(JEPLMCtab[,2]),
            sum(JEPLMCtab[2,2]))
JEPLMCperc <- c(sum(JEPLMCtab[2,])/sum(JEPLMCtab),
                sum(JEPLMCtab[,2])/sum(JEPLMCtab),
                sum(JEPLMCtab[2,2])/sum(JEPLMCtab[2,]))

PS <- c(sum(PStab[2,]),
        sum(PStab[,2]),
        sum(PStab[2,2]))
PSperc <- c(sum(PStab[2,])/sum(PStab),
            sum(PStab[,2])/sum(PStab),
            sum(PStab[2,2])/sum(PStab[2,]))


cbind(labels, JPSP, JPSPperc, JEPLMC, JEPLMCperc, PS, PSperc)

# Replication sig | original sig
35/97

# Deviation from uniformity in nonsignificant replication studies
FisherMethod(x = MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..R.)],
             id = 1,
             alpha = .05)

# Means p-value distributions
mean(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])
mean(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])

# Dependent t-test p-values
t.test(x = MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
       y = MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
       paired = TRUE)

# Wilcoxon signed-rank test p-values
wilcox.test(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
            MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)],
            alternative="two.sided")

# Quantiles
summary(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])[c(2, 3, 5)]
summary(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])[c(2, 3, 5)]

# Figure S1 (CDF PVAL)
setEPS()
postscript("figure s1.eps", width = 7, height = 8) # change file name
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
abline(v = .05, lty = 2)
lines(ecdf(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))
legend(x=.5,y=.3,legend=c(paste('Original p-values, k = ',
                                length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])),
                          paste('Replication p-values, k = ',
                                length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))),
       cex=1,
       lty=c(1,1), bty = 'n',
       col = c("grey","black"),box.lwd=0)
dev.off()

# Figure S2 (PDF Pvalues)
setEPS()
postscript("figure s2.eps", width = 7, height = 8) # change file name
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
abline(v = .05, lty = 2)
lines(density(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))
legend(x=.4,y=7,legend=c(paste('Original p-values, k = ',
                               length(MASTER$T_pval_USE..O.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)])),
                         paste('Replication p-values, k = ',
                               length(MASTER$T_pval_USE..R.[!is.na(MASTER$T_pval_USE..O.) & !is.na(MASTER$T_pval_USE..R.)]))),
       cex=1,
       lty=c(1,1), bty = 'n',
       col = c("grey","black"),box.lwd=0)
dev.off()

####################################################
# Comparing original and replication effect sizes. #
####################################################
# Written by CHJ Hartgerink

# Number of pairs
sum(!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.))

# Descriptive original studies
mean(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])
sd(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])
# Descriptive replication studies
mean(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])
sd(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])

# Dependent t-test effects (r values)
t.test(x = MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
       y = MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
       paired = TRUE)

# Wilcox test effects (r values)
wilcox.test(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
            MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
            alternative="two.sided")

# Binomial test to see if replicated effect is larger than original
temp <- MASTER$T_O_larger
prop <- sum(temp[!is.na(temp)])/length(temp[!is.na(temp)])
binom.test(x = sum(temp[!is.na(temp)]), n = length(temp[!is.na(temp)]), 
           p = .5, alternative = "two.sided")

# Spearman correlation original and replicated effect sizes
cor(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
    MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)],
    method = "spearman")

setEPS()
postscript("figure s3.eps", width = 11, height = 7) # change file name
# Histogram effects 
par(mfrow=c(1,2))
hist1 <- hist(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)], breaks=15)
hist2 <- hist(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)], breaks=20)
plot(hist1, xlim = c(-.6, 1.2), ylim = c(0, 50), xlab = "Effect size r",
     main = "Effect histograms")
plot(hist2, add = TRUE, col = "grey")
legend(x = .4, y = 25,
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
legend(x=.45,y=.5,legend=c(paste("Original, k = ",
                                 length(MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)])),
                           paste("Replication, k = ",
                                 length(MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]))),
       cex=1,
       lty=c(1,1), bty = 'n',
       col = c("grey","black"),box.lwd=0)
dev.off()

##############################################################
# Evaluating replication effect against original effect size #
##############################################################
# Plot by CHJ Hartgerink

setEPS()
postscript("figure s4.eps", width = 7, height = 8) # change file name
plot(y = MASTER$T_r..R., x = MASTER$T_r..O., xlab = "Effect size r (original)",
     ylab = "Effect size r (replication)", col = "white", xlim = c(-.3, 1), ylim = c(-.3, 1),
     frame.plot=F,
     xaxs="i",
     yaxs="i"
)
abline(v=0, col = "grey")
abline(h=0, col = "grey")
points(y = MASTER$T_r..R.[MASTER$T_sign_O == 0 & MASTER$T_sign_R == 0],
       x = MASTER$T_r..O.[MASTER$T_sign_O == 0 & MASTER$T_sign_R == 0], 
       pch = 4, col = 1)
points(y = MASTER$T_r..R.[MASTER$T_sign_O == 1 & MASTER$T_sign_R == 0],
       x = MASTER$T_r..O.[MASTER$T_sign_O == 1 & MASTER$T_sign_R == 0],
       pch = 21, col = 10)
points(y = MASTER$T_r..R.[MASTER$T_sign_O == 1 & MASTER$T_sign_R == 1],
       x = MASTER$T_r..O.[MASTER$T_sign_O == 1 & MASTER$T_sign_R == 1], 
       pch = 10, col = 3)
lines(x = seq(-.5, 2, .0001), y = seq(-.5, 2, .0001), type = 'l')

# Regression line
x1 <- MASTER$T_r..O.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]
y1 <- MASTER$T_r..R.[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.)]

# Linear
xx <- lm(y1 ~ x1)
lines(loess.smooth(x = x1, y = y1), lty = 2)
curve(expr = (xx$coefficients[1] + xx$coefficients[2] * x), from = min(x1),
      to = max(x1), add = TRUE, col = "blue")

legend(x=-.2,y=.8,legend=c('Both nonsignificant',
                           'Original significant',
                           'Both significant',
                           'Repl. predicted by orig.',
                           'Loess curve'),
       cex=1,
       lty=c(0, 0, 0, 1, 2), bty = 'n', pch = c(4, 21, 10, NA, NA),
       col = c("black", 10, 3, "blue", "black"), box.lwd=0)
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

### Create the right order of the variable "Surprise of outcome (R)"
fac.sur.out <- factor(MASTER$Surprise.of.outcome..R., levels = c("Results were exactly as anticipated", "Results were slightly surprising", "Results were somewhat surprising",
                                                                 "Results were moderately surprising", "Results were extremely surprising"))

###########################################
### Prepare variables for meta-analyses ###
###########################################

### Function for standardizing variables
stand <- function(x) {
  res <- (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(res)
}

# a. PUBLISHING JOURNAL AND SUBDISCIPLINE

### Create dummy variables for "Publishing journal and subdiscipline"
### JPSP is reference category because it has the most cases
d.JEP <- ifelse(jour == 1, 1, 0)
d.PSCog <- ifelse(jour == 3, 1, 0)
d.PSSoc <- ifelse(jour == 4, 1, 0)

# b. IMPORTANCE OF THE EFFECT

### Standardizing "Citation count, paper (O)"
st.impa <- stand(MASTER$Citation.count..paper..O.)

### Standardizing "Exciting/important effect"
st.exci <- stand(as.numeric(levels(MASTER$Exciting.result..O.))[MASTER$Exciting.result..O.])

### Creating scale
sc.impo <- (st.impa + st.exci)/2

# c. SURPRISING EFFECT

### Standardizing "Surprising effect" and creating scale
sc.surp <- stand(as.numeric(levels(MASTER$Surprising.result..O.))[MASTER$Surprising.result..O.])

# d. EXPERIENCE AND EXPERTISE OF ORIGINAL TEAM

### Taking the average and then standardizing "Institution prestige of 1st author and senior author" 
ave.pres <- (MASTER$Institution.prestige..1st.author..O.+MASTER$Institution.prestige..senior.author..O.)/2
st.pres <- stand(ave.pres)

### Standardizing "Citation Count, 1st author (O)"
st.impa.1st <- stand(MASTER$Citation.Count..1st.author..O.)

### Standardizing "Citation count, senior author (O)"
st.impa.sen <- stand(MASTER$Citation.count..senior.author..O.)

### Creating scale
sc.expe1 <- (st.pres + st.impa.1st + st.impa.sen)/3

# e. CHALLENGE OF CONDUCTING REPLICATION

### Standardizing "Perceived expertise required"
st.expe <- stand(as.numeric(fac.expe))

### Standardizing "Perceived opportunity for expectancy biases"
st.oppo.expe <- stand(as.numeric(fac.oppo.expe))

### Standardizing "Perceived opportunity for impact of lack of diligence"
st.oppo.dili <- stand(as.numeric(fac.oppo.dili))

### Create scale
sc.chal <- (st.expe + st.oppo.expe + st.oppo.dili)/3

# f. EXPERIENCE AND EXPERTISE OF REPLICATION TEAM

### Standardizing "Position of senior member of replication team"
st.posi <- stand(as.numeric(fac.posi))

### Standardizing "Highest degree of replication team's senior member"
st.degr <- stand(as.numeric(fac.degr))

### Standardizing "Replication team domain expertise"
st.doma <- stand(as.numeric(fac.doma))

### Standardizing "Replication team method expertise"
st.meth <- stand(as.numeric(fac.meth))

### Standardizing "Replication team senior member's total publications"
st.publ <- stand(MASTER$Total.publications..R.)

### Standardizing "Replication team senior member's total publications and total number of peer-reviewed articles"
st.peer <- stand(MASTER$Peer.reviewed.articles..R.)

### Standardizing "Replication team senior member's total citations"
st.cita <- stand(MASTER$Citations..R.)

### Create scale
sc.expe2 <- (st.posi + st.degr + st.doma + st.meth + st.publ + st.cita)/6

# g. SELF-ASSESSED QUALITY OF REPLICATION

### Standardizing "Self-assessed quality of replication"
st.impl <- stand(as.numeric(fac.impl))

### Standardizing "Self-assessed data collection quality of replication"
st.data <- stand(as.numeric(fac.data))

### Standardizing "Self-assessed replication similarity to original"
st.repl <- stand(as.numeric(fac.repl))

### Standardizing "Self-assessed difficulty of implementation"
st.diff <- stand(as.numeric(fac.diff))

### Create a scale
sc.self <- (st.impl + st.data + st.repl + st.diff)/4

###########################################
### Transforming correlations to Fisher ###
###########################################

ri.o <- MASTER$T_r..O.
ri.r <- MASTER$T_r..R.
N.o <- MASTER$T_df2..O.+2
N.r <- MASTER$T_df2..R.+2

### Partial correlation, so degrees of freedom plus 2 in order to get N
N.o[MASTER$ID == 82] <- MASTER$T_df1..O.[82]+2
N.r[MASTER$ID == 82] <- MASTER$T_df1..R.[82]+2

### Correlation
N.o[MASTER$ID == 120] <- MASTER$T_N..O.[120]
N.r[MASTER$ID == 120] <- MASTER$T_N..R.[120]
N.o[MASTER$ID == 154] <- MASTER$T_N..O.[154]
N.r[MASTER$ID == 154] <- MASTER$T_N..R.[154]
N.o[MASTER$ID == 155] <- MASTER$T_N..O.[155]
N.r[MASTER$ID == 155] <- MASTER$T_N..R.[155]

### t
N.o[MASTER$ID == 121] <- MASTER$T_N..O.[121]
N.r[MASTER$ID == 121] <- MASTER$T_N..R.[121]

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
### Coverage F(>1,df2) and Chi2     ###
#######################################
# Written by MALM van Assen

tol <- 1e-7
xm <- 0

df1.or <- df2.or <- F.or <- df1.rep <- df2.rep <- F.rep <- 1:18
ncp.L <- ncp.U <- ncp.o <- in.ci <- 1:18

### study 12
df1.or[1] <- 2
df2.or[1] <- 92
F.or[1] <- 3.13
df1.rep[1] <- 2
df2.rep[1] <- 232
F.rep[1] <- 1.63

### study 13
df1.or[2] <- 2
df2.or[2] <- 68
F.or[2] <- 41.59
df1.rep[2] <- 2
df2.rep[2] <- 68
F.rep[2] <- 41.603

### study 17
df1.or[3] <- 2
df2.or[3] <- 76
F.or[3] <- 8.67
df1.rep[3] <- 1.58
df2.rep[3] <- 72.4
F.rep[3] <- 19.48

### study 22
df1.or[4] <- 3
df2.or[4] <- 93
F.or[4] <- 5.23
df1.rep[4] <- 2.33
df2.rep[4] <- 90
F.rep[4] <- 0.38

### study 43
df1.or[5] <- 2
df2.or[5] <- 64
F.or[5] <- 10.17
df1.rep[5] <- 2
df2.rep[5] <- 72
F.rep[5] <- 1.97

### study 46
df1.or[6] <- 21
df2.or[6] <- 230025
F.or[6] <- 118.15
df1.rep[6] <- 21
df2.rep[6] <- 455304
F.rep[6] <- 261.93

### study 50
df1.or[7] <- 2
df2.or[7] <- 92
F.or[7] <- 4.36
df1.rep[7] <- 2
df2.rep[7] <- 103
F.rep[7] <- 2.601

### study 55
df1.or[8] <- 2
df2.or[8] <- 54
F.or[8] <- 3.19
df1.rep[8] <- 2
df2.rep[8] <- 68
F.rep[8] <- 0.3

### study 64
df1.or[9] <- 2
df2.or[9] <- 76
F.or[9] <- 21.57
df1.rep[9] <- 2
df2.rep[9] <- 65
F.rep[9] <- 0.865

### study 80
df1.or[10] <- 2
df2.or[10] <- 43
F.or[10] <- 3.36
df1.rep[10] <- 2
df2.rep[10] <- 67
F.rep[10] <- 1.7

### study 86
df1.or[11] <- 2
df2.or[11] <- 82
F.or[11] <- 4.05
df1.rep[11] <- 2
df2.rep[11] <- 137
F.rep[11] <- 1.99

### study 117
df1.or[12] <- 18
df2.or[12] <- 660
F.or[12] <- 16.31
df1.rep[12] <- 18
df2.rep[12] <- 660
F.rep[12] <- 12.98

### study 132
df1.or[13] <- 3
df2.or[13] <- 69
F.or[13] <- 5.15
df1.rep[13] <- 1.48
df2.rep[13] <- 41.458
F.rep[13] <- 1.401

### study 139
df1.or[14] <- 3
df2.or[14] <- 9
F.or[14] <- 8.5
df1.rep[14] <- 3
df2.rep[14] <- 12
F.rep[14] <- 13.06

### study 140
df1.or[15] <- 2
df2.or[15] <- 81
F.or[15] <- 4.97
df1.rep[15] <- 2
df2.rep[15] <- 122
F.rep[15] <- 0.24

### study 142
df1.or[16] <- 2
df2.or[16] <- 162
F.or[16] <- 192.89
df1.rep[16] <- 2
df2.rep[16] <- 174
F.rep[16] <- 252.83

### study 143
df1.or[17] <- 4
df2.or[17] <- 108
F.or[17] <- 3.67
df1.rep[17] <- 4
df2.rep[17] <- 150
F.rep[17] <- 0.58

### Added later, after reviews, before re-submitting to Science [July 16, 2015]
### study 25
df1.or[18] <- 3
df2.or[18] <- 48
F.or[18] <- 9.14
df1.rep[18] <- 3
df2.rep[18] <- 59
F.rep[18] <- 5.681

### loop
for (i in 1:length(F.or)) {
  df1.o <- df1.or[i]
  df2.o <- df2.or[i]
  F.o <- F.or[i]
  df1.r <- df1.rep[i]
  df2.r <- df2.rep[i]
  F.r <- F.rep[i]
  
  ### ncp lower bound
  if (pf(F.r,df1.r,df2.r,0) < .975)
  {ncp.L[i] <- 0} else
  {
    x0 <- 0
    x1 <- df1.r*F.r
    ym <- 1  
    while(abs(ym-0.975) > tol) {
      xm <- (x0+x1)/2
      ym <- pf(F.r,df1.r,df2.r,xm)
      if (ym > 0.975) x0 <- xm
      if (ym < 0.975) x1 <- xm  
    }
    ncp.L[i] <- xm
  }
  
  ### ncp upper bound
  x0 <- df1.r*F.r
  x1 <- 20*df1.r*F.r
  ym <- 1  
  while(abs(ym-0.025) > tol) {
    xm <- (x0+x1)/2
    ym <- pf(F.r,df1.r,df2.r,xm)
    if (ym > 0.025) x0 <- xm
    if (ym < 0.025) x1 <- xm
  }
  ncp.U[i] <- xm
  
  ### check if original is in ci of replication
  ncp.o[i] <- F.o*df1.o*(df2.o-2)/df2.o-df1.o
  in.ci[i] <- ( (ncp.L[i] < ncp.o[i]) & (ncp.U[i] > ncp.o[i]) )
}

cbind(ncp.L,ncp.o,ncp.U,in.ci)
sum(in.ci)
mean(in.ci)

### ch2
## if probability calculated with pchisq is between .025
## and .975 then the ncp of original is in ci of replication

## Study 73
chi2.o <- 3.85
chi2.r <- 4.8
pchisq(chi2.r,1,chi2.o-1)

## Study 84
chi2.o <- 13.18
chi2.r <- 7.1
pchisq(chi2.r,1,chi2.o-1)

## Study 104
chi2.o <- 3.83
chi2.r <- 0.387
pchisq(chi2.r,1,chi2.o-1)

## Study 165
chi2.o <- 4.51
chi2.r <- 1.57
pchisq(chi2.r,1,chi2.o-1)

(sum(in.ci)+4)/(length(in.ci)+4) # Original study within CI of replication

#######################################
### Select studies for the analyses ###
#######################################

# Written by RCM van Aert

df <- data.frame(ID = MASTER$ID, stat = as.character(MASTER$T_Test.Statistic..R.), df1 = MASTER$T_df1..O., yi, sei, fis.o, sei.o, N.o, pval.o, fis.r, sei.r, N.r, pval.r, 
                 d.JEP, d.PSCog, d.PSSoc, sc.impo, sc.surp, sc.expe1, sc.chal, sc.expe2, sc.self, power.r = as.numeric(levels(MASTER$Power..R.))[MASTER$Power..R.],
                 N.o.tab = MASTER$T_N_O_for_tables, N.r.tab = MASTER$T_N_R_for_tables, replic = as.character(MASTER$Replicate..R.))

### Select: F(df1 = 1, df2), t, and r
sub <- subset(df, (df$stat == "F" & df$df1 == 1) | df$stat == "t" | df$stat == "r")

### Remove rows when NA on yi
final <- sub[!is.na(sub$yi) & !is.na(sub$sei), ]

################################################################################################
### Table 1 / "Combining original and replication effect sizes for cumulative evidence ###
################################################################################################

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
sum(pval)
sum(pval)/(length(pval)+2) # Proportion of statistically significant studies plus two because of two non-significant p-values for odds rations (ID = 84 and 165)

### Per discipline
mean(cor[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0])
mean(cor[final$d.JEP == 1])
mean(cor[final$d.PSSoc == 1])
mean(cor[final$d.PSCog == 1])

sd(cor[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0])
sd(cor[final$d.JEP == 1])
sd(cor[final$d.PSSoc == 1])
sd(cor[final$d.PSCog == 1])

sum(pval[final$d.JEP == 0 & final$d.PSSoc == 0 & final$d.PSCog == 0])/(length(pval[final$d.JEP == 0 # Plus one because of one odds ratio (ID = 84)
                                                                                   & final$d.PSSoc == 0 & final$d.PSCog == 0])+1)
sum(pval[final$d.JEP == 1])/length(pval[final$d.JEP == 1])
sum(pval[final$d.PSSoc == 1])/(length(pval[final$d.PSSoc == 1])+1) # Plus one because of one odds ratio (ID = 165)
sum(pval[final$d.PSCog == 1])/length(pval[final$d.PSCog == 1])

### Subset of data with all cases which were replicated
tmp <- subset(df, is.na(MASTER$T_N_O_for_tables) == FALSE)
mean(tmp$power.r, na.rm = TRUE)

### Subset of data with all cases published in JPSP
tmp.JPSP <- subset(tmp, tmp$d.JEP == 0 & tmp$d.PSSoc == 0 & tmp$d.PSCog == 0)
mean(tmp.JPSP$power.r, na.rm = TRUE)

### Subset of data with all cases published in JEP
tmp.JEP <- subset(tmp, tmp$d.JEP == 1)
mean(tmp.JEP$power.r, na.rm = TRUE)

### Subset of data with all cases published in PSSoc
tmp.PSSoc <- subset(tmp, tmp$d.PSSoc == 1)
mean(tmp.PSSoc$power.r, na.rm = TRUE)

### Subset of data with all cases published in PSCog
tmp.PSCog <- subset(tmp, tmp$d.PSCog == 1)
mean(tmp.PSCog$power.r, na.rm = TRUE)

### Subset of data with complete data on effect size
tmp <- subset(df, is.na(MASTER$T_r..R.) == FALSE)
summary(tmp$N.o.tab)
summary(tmp$N.r.tab)

### Subset of complete data JPSP
tmp.JPSP <- subset(tmp, tmp$d.JEP == 0 & tmp$d.PSSoc == 0 & tmp$d.PSCog == 0)
summary(tmp.JPSP$N.o.tab)
summary(tmp.JPSP$N.r.tab)

### Subset of complete data JEP
tmp.JEP <- subset(tmp, tmp$d.JEP == 1)
summary(tmp.JEP$N.o.tab)
summary(tmp.JEP$N.r.tab)

### Subset of complete data PSSoc
tmp.PSSoc <- subset(tmp, tmp$d.PSSoc == 1)
summary(tmp.PSSoc$N.o.tab)
summary(tmp.PSSoc$N.r.tab)

### Subset of complete data PSCog
tmp.PSCog <- subset(tmp, tmp$d.PSCog == 1)
summary(tmp.PSCog$N.o.tab)
summary(tmp.PSCog$N.r.tab)

### % subjective "yes" to "Did it replicate?"
tmp <- subset(df, is.na(MASTER$T_N_O_for_tables) == FALSE) # Subset of data with all cases which were replicated
sum(tmp$replic == "yes")/length(tmp$replic)
sum(tmp$replic[tmp$d.JEP == 0 & tmp$d.PSSoc == 0 & tmp$d.PSCog == 0] == "yes")/length(tmp$replic[tmp$d.JEP == 0 & tmp$d.PSSoc == 0 & tmp$d.PSCog == 0])
sum(tmp$replic[tmp$d.JEP == 1] == "yes")/length(tmp$replic[tmp$d.JEP == 1])
sum(tmp$replic[tmp$d.PSSoc == 1] == "yes")/length(tmp$replic[tmp$d.PSSoc == 1])
sum(tmp$replic[tmp$d.PSCog == 1] == "yes")/length(tmp$replic[tmp$d.PSCog == 1])

### "original effect size within replication 95% CI"
### How often is original study within CI of replication
### Create confidence interval for replications
ci.lb <- fis.r-qnorm(.975)*sei.r
ci.ub <- fis.r+qnorm(.975)*sei.r

tmp <- in.ci <- rep(NA, length(ci.lb))

for(i in 1:length(fis.r)) {
  
  if (is.na(fis.o[i]) == TRUE) {
    tmp[i] <- NA
  } else if (any(is.na(c(ci.lb[i], ci.ub[i])) == TRUE)) {
    tmp[i] <- NA
  } else if (fis.o[i] > ci.lb[i] & fis.o[i] < ci.ub[i]) {
    tmp[i] <- TRUE
  } else { tmp[i] <- FALSE }
  
}

### Select only studies with test statistic t or F and df1 = 1
dat <- data.frame(ID = MASTER$ID, stat = as.character(MASTER$T_Test.Statistic..O.), df1 = MASTER$T_df1..O., tmp)
sub <- subset(dat, (dat$stat == "F" & dat$df1 == 1) | dat$stat == "t" | dat$stat == "r")
in.ci[sub$ID] <- sub$tmp

### Store results for other statistics
in.ci[c(22,43,46,64,132,140,143)] <- FALSE
in.ci[c(12,13,17,50,55,80,86,117,139,142,73,84,104,165)] <- TRUE

noNA <- in.ci[is.na(in.ci) == FALSE] # Remove NAs
sum(noNA)/length(noNA) # How often original study is within CI of replication

# How often original study is within CI of replication: per journal and discipline
sum(in.ci[df$d.JEP == 0 & df$d.PSSoc == 0 & df$d.PSCog == 0], na.rm = TRUE)/sum(is.na(in.ci[df$d.JEP == 0 & df$d.PSSoc == 0 & df$d.PSCog == 0]) == FALSE)
sum(in.ci[df$d.JEP == 1], na.rm = TRUE)/sum(is.na(in.ci[df$d.JEP == 1]) == FALSE)
sum(in.ci[df$d.PSSoc == 1], na.rm = TRUE)/sum(is.na(in.ci[df$d.PSSoc == 1]) == FALSE)
sum(in.ci[df$d.PSCog == 1], na.rm = TRUE)/sum(is.na(in.ci[df$d.PSCog == 1]) == FALSE)

### Two meta-analyses per pair for odds ratios
### Study 84
logodds1 <- log((63*70)/(48*55))
var1 <- 1/63 + 1/55 + 1/48 + 1/70
logodds2 <- log((231*350)/(213*352))
var2 <- 1/231 + 1/352 + 1/213 + 1/350

rma(yi = c(logodds1, logodds2), vi = c(var1, var2), method = "FE")

# study 165
logodds1 <- log((15*21)/(12*8))
var1 <- 1/15 + 1/21 + 1/12 + 1/8
logodds2 <- log((11*10)/(14*16))
var2 <- 1/11 + 1/10 + 1/14 + 1/16

rma(yi = c(logodds1, logodds2), vi = c(var1, var2), method = "FE")

#########################################################################################
### Meta-analyses of difference of effect size between original and replication study ###
#########################################################################################

### Meta-analysis of null model
res <- rma(yi = final$yi, sei = final$sei, method = "REML")
res

setEPS()
postscript("figure s7.eps", width = 7, height = 8) # change file name
funnel(res, main = "Funnel plot based on difference original and replication study")
dev.off()

### Meta-analysis with a. PUBLISHING JOURNAL AND SUBDISCIPLINE as moderator (JPSP is reference category)
rma(yi = final$yi, sei = final$sei, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "REML")

### Meta-analysis with sei.o as moderator
rma(yi = final$yi, sei = final$sei, mods = ~ final$sei.o, method = "REML")

### Meta-analysis with sei.o as moderator with "ML" as estimator for tau2 (model comparison)
res0 <- rma(yi = final$yi, sei = final$sei, mods = ~ final$sei.o, method = "ML")
res0

### Meta-analysis with a. PUBLISHING JOURNAL AND SUBDISCIPLINE and sei.o as moderators (JPSP is reference category)
rma(yi = final$yi, sei = final$sei, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc + final$sei.o, method = "REML")

### Meta-analysis with a. PUBLISHING JOURNAL AND SUBDISCIPLINE and sei.o as moderators (JPSP is reference category) with "ML" as estimator for tau2 (model comparison)
res1 <- rma(yi = final$yi, sei = final$sei, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc + final$sei.o, method = "ML")
res1

### Comparing models with and without discipline
anova(res0, res1)

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

#########################################################################################
### Meta-analyses of all original study effects, and of all replication study effects ###
#########################################################################################

### Meta-analysis of null model
res <- rma(yi = final$fis.o, sei = final$sei.o, method = "REML")
res

setEPS()
postscript("figure s5.eps", width = 7, height = 8) # change file name
funnel(res, main = "Funnel plot based on original studies")
dev.off()

### Meta-analysis with se in original study as moderator
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o, method = "REML")

### Meta-analysis with se in original study as moderator with "ML" as estimator for tau2 (model comparison)
res0 <- rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o, method = "ML")
res0

### Meta-analysis with se in original study and dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o + final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "REML")

### Meta-analysis with se in original study and dummy variables for journal and discipline (JPSP is reference category) with "ML" as estimator for tau2 (model comparison)
res1 <- rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o + final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "ML")
res1

### Comparing models with and without discipline
anova(res0, res1)

### Meta-analysis with dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "REML")

### Meta-analysis with se in original study, dummy variables for journal and discipline (JPSP is reference category), and its interactions as moderators
rma(yi = final$fis.o, sei = final$sei.o, mods = ~ final$sei.o + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$sei.o*final$d.JEP + final$sei.o*final$d.PSSoc + final$sei.o*final$d.PSCog, method = "REML")

### Meta-analyses based on only replication studies
### Meta-analysis of null model
res <- rma(yi = final$fis.r, sei = final$sei.r, method = "REML")
res

setEPS()
postscript("figure s6.eps", width = 7, height = 8) # change file name
funnel(res, main = "Funnel plot based on replication studies")
dev.off()

### Meta-analysis with se in replication study as moderator
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r, method = "REML")

### Meta-analysis with se in replication study as moderator with "ML" as estimator for tau2 (model comparison)
res0 <- rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r, method = "ML")
res0

### Meta-analysis with se in replication study and dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "REML")

### Meta-analysis with se in replication study and dummy variables for journal and discipline (JPSP is reference category) with "ML" as estimator for tau2 (model comparison)
res1 <- rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "ML")
res1

### Comparing models with and without discipline
anova(res0, res1)

### Meta-analysis with se in replication study and dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "REML")

### Meta-analysis with dummy variables for journal and discipline (JPSP is reference category)
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$d.JEP + final$d.PSCog + final$d.PSSoc, method = "REML")

### Meta-analysis with se in replication study, dummy variables for journal and discipline (JPSP is reference category), and its interactions as moderators
rma(yi = final$fis.r, sei = final$sei.r, mods = ~ final$sei.r + final$d.JEP + final$d.PSCog + final$d.PSSoc + final$sei.r*final$d.JEP + final$sei.r*final$d.PSSoc + final$sei.r*final$d.PSCog, method = "REML")

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
round(tab, 3)

### Check how often effect size original study is within CI of meta-analysis
in.ci.meta <- numeric()

for(i in 1:length(final$fis.o)) {
  
  if(final$fis.o[i] > ci.lb.meta[i] & final$fis.o[i] < ci.ub.meta[i]) {
    in.ci.meta[i] <- TRUE
  } else { in.ci.meta[i] <- FALSE }
  
}

sum(in.ci.meta)/length(in.ci.meta) # Proportion of times the original study is within the CI of meta-analysis

######################################################################################
### Predicting how often effect size of original study is within CI of replication ###
######################################################################################

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
rcorr(mat, type = "spearman")

###########
# TABLE 1 #
# https://docs.google.com/spreadsheets/d/16aqIekNerZcflSJwM7dOiXnN24-KvZqaa3eTg4js41U/edit#gid=0
###########

# Overall 
sel <- MASTER[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & MASTER$T_sign_O == 1, ]

# Column B
# Relations tested
reltest <- dim(sel)[1]
# Relations both significant
relsig <- sum((sel$T_sign_O) & (sel$T_sign_R), na.rm = TRUE)
cat(
  paste0("Column B Overall ",
         round(relsig, 0),
         " / ",
         round(reltest, 0)
  )
)

# Column C
# Percent
cat(paste0("Column C overall ", round((relsig / reltest), 2)))

# Column D
# Mean
temp1 <- mean(sel$T_r..O., na.rm = TRUE)
# SD
temp2 <- sd(sel$T_r..O., na.rm = TRUE)

cat(paste0("Column D overall ", round(temp1, 3), " (", round(temp2, 3), ")"))

# Column F
# Mean
temp1 <- mean(sel$T_r..R., na.rm = TRUE)
# SD
temp2 <- sd(sel$T_r..R., na.rm = TRUE)

cat(paste0("Column F overall ", round(temp1, 3), " (", round(temp2, 3), ")"))


# Per journal 
journals <- c("JEPLMC", "JPSP", "PS Cognitive", "PS social", "PS other")
for(journal in c(2,1,4,3,5)){
  
  sel <- MASTER[!is.na(MASTER$T_sign_O) & !is.na(MASTER$T_sign_R) & MASTER$T_sign_O == 1 & jour == journal,]
  
  # Column B
  # Relations tested
  reltest <- dim(sel)[1]
  # Relations both significant
  relsig <- sum((sel$T_sign_O) & (sel$T_sign_R), na.rm = TRUE)
  cat(
    paste0("Column B ", journals[journal], " ",
           round(relsig, 0),
           " / ",
           round(reltest, 0)
    )
  )
  cat("\n")
  
  # Column C
  # Percent
  cat(paste0("Column C ", journals[journal], " ",
             as.character(round((relsig / reltest), 2))))
  cat("\n")
  
  # Column D
  # Mean
  temp1 <- mean(sel$T_r..O., na.rm = TRUE)
  # SD
  temp2 <- sd(sel$T_r..O., na.rm = TRUE)
  
  cat(paste0("Column D ", journals[journal], " ",
             round(temp1, 2), " (", round(temp2, 2), ")"))
  cat("\n")
  # Column F
  # Mean
  temp1 <- mean(sel$T_r..R., na.rm = TRUE)
  # SD
  temp2 <- sd(sel$T_r..R., na.rm = TRUE)
  
  cat(paste0("Column F ", journals[journal], " ",round(temp1, 2), " (", round(temp2, 2), ")"))
  cat("\n")
}

###################
##### TABLE 2 #####
###################

# Notes:
# p-value = recalculated p-value
# effect size = correlation coefficient because we use Spearman's rank correlation transformation is not required
# T_pval_USE..R is used to create a subset of replicated studies

### Variables in the columns
rows <- cbind(MASTER$T_pval_USE..O., MASTER$T_r..O., MASTER$T_N_O_for_tables, df$sc.impo, df$sc.surp, df$sc.expe1, MASTER$T_pval_USE..R, MASTER$T_r..R., df$power.r, 
              MASTER$T_N_R_for_tables, df$sc.chal, df$sc.expe2, df$sc.self, MASTER$T_pval_USE..R)
colnames(rows) <- c("p-value", "Original Effect size", "Original sample size", "Importance of effect", "Surprising effect", "Experience and expertise of team O", "Replication p-value",
                    "Replication Effect size", "Power", "Replication Sample size", "Challenge of conducting replication", "Experience and expertise of team R", "Self-assessed quality of replication", "sub")

### "Replications p < 0.05 in original direction (all signicant replication studies are in the same direction as the original study)"
rep.dir <- ifelse(MASTER$T_sign_O == 0, yes = NA, no = MASTER$T_sign_R)
mat <- cbind(rep.dir, rows)
colnames(mat) <- c("rep.dir", "p-value", "Original Effect size", "Original sample size", "Importance of effect", "Surprising effect", "Experience and expertise of team O", "Replication p-value",
                   "Replication Effect size", "Power", "Replication Sample size", "Challenge of conducting replication", "Experience and expertise of team R", "Self-assessed quality of replication", "sub")

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "Effect size difference" (Fisher z)
es.dif <- df$fis.o - df$fis.r

mat <- cbind(es.dif, rows)
colnames(mat) <- c("Effect size Difference", "p-value", "Original Effect size", "Original sample size", "Importance of effect", "Surprising effect", "Experience and expertise of team O", "Replication p-value",
                   "Replication Effect size", "Power", "Replication Sample size", "Challenge of conducting replication", "Experience and expertise of team R", "Self-assessed quality of replication", "sub")
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "Meta-analytic estimate" (Fisher z)
mat <- cbind(es.meta.all, rows)
colnames(mat) <- c("Meta-analytic estimate", "p-value", "Original Effect size", "Original sample size", "Importance of effect", "Surprising effect", "Experience and expertise of team O", "Replication p-value",
                   "Replication Effect size", "Power", "Replication Sample size", "Challenge of conducting replication", "Experience and expertise of team R", "Self-assessed quality of replication", "sub")
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "original effect size within replication 95% CI"
### How often is original study within CI of replication
### Create confidence interval for replications
ci.lb <- fis.r-qnorm(.975)*sei.r
ci.ub <- fis.r+qnorm(.975)*sei.r

tmp <- in.ci <- rep(NA, length(ci.lb))

for(i in 1:length(fis.r)) {
  
  if (is.na(fis.o[i]) == TRUE) {
    tmp[i] <- NA
  } else if (any(is.na(c(ci.lb[i], ci.ub[i])) == TRUE)) {
    tmp[i] <- NA
  } else if (fis.o[i] > ci.lb[i] & fis.o[i] < ci.ub[i]) {
    tmp[i] <- TRUE
  } else { tmp[i] <- FALSE }
  
}

### Select only studies with test statistic t or F and df1 = 1
dat <- data.frame(ID = MASTER$ID, stat = as.character(MASTER$T_Test.Statistic..R.), df1 = MASTER$T_df1..R., tmp)
sub <- subset(dat, (dat$stat == "F" & dat$df1 == 1) | dat$stat == "t" | dat$stat == "r")
in.ci[sub$ID] <- sub$tmp

sum(in.ci, na.rm = TRUE) # Number of times the replication study contained the effect size of the original study of the studies were the se could be computed

### Store results for other statistics
in.ci[c(22,43,46,64,132,140,143)] <- FALSE
in.ci[c(12,13,17,50,55,80,86,117,139,142,73,84,104,165)] <- TRUE

mat <- cbind(in.ci, rows)
colnames(mat) <- c("Original in CI", "p-value", "Original Effect size", "Original sample size", "Importance of effect", "Surprising effect", "Experience and expertise of team O", "Replication p-value",
                   "Replication Effect size", "Power", "Replication Sample size", "Challenge of conducting replication", "Experience and expertise of team R", "Self-assessed quality of replication", "sub")

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "subjective 'yes' to 'Did it replicate?'"
replic <- vector(mode = "logical", length = length(levels(MASTER$Replicate..R.)[MASTER$Replicate..R.]))
for (i in 1:length(levels(MASTER$Replicate..R.)[MASTER$Replicate..R.])) {
  if (levels(MASTER$Replicate..R.)[MASTER$Replicate..R.][i] == "yes") { replic[i] <- 1
  } else { replic[i] <- 0 }
}

mat <- cbind(replic, rows)
colnames(mat) <- c("Did it replicate?", "p-value", "Original Effect size", "Original sample size", "Importance of effect", "Surprising effect", "Experience and expertise of team O", "Replication p-value",
                   "Replication Effect size", "Power", "Replication Sample size", "Challenge of conducting replication", "Experience and expertise of team R", "Self-assessed quality of replication", "sub")
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

#####################
##### TABLE S2  #####
#####################

mat <- cbind(rep.dir, es.dif, es.meta.all, in.ci, replic, MASTER$T_pval_USE..R)
colnames(mat) <- c("rep.dir", "Effect Size Difference", "Meta-analytic estimate", "Original in CI", "Did it replicate?", "sub")

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

rcorr(sub, type = "spearman")$r # Create table with correlations

####################
##### TABLE S3 #####
####################

### Create data frame
dat <- data.frame(Original.effect.size = MASTER$T_r..O., Original.pvalue = MASTER$T_pval_USE..O., Original.sample.size = MASTER$T_N_O_for_tables, 
                  Inst.1st.author = MASTER$Institution.prestige..1st.author..O., Inst.sen.author = MASTER$Institution.prestige..senior.author..O.,
                  Cit.1st.author = MASTER$Citation.Count..1st.author..O., Cit.sen.author = MASTER$Citation.count..senior.author..O.,
                  Cit.impact = MASTER$Citation.count..paper..O., concep.repl = MASTER$Internal.conceptual.replications..O.,
                  dir.repl = MASTER$Internal.direct.replications..O., surp.res = as.numeric(levels(MASTER$Surprising.result..O.))[MASTER$Surprising.result..O.],
                  exci.res = as.numeric(levels(MASTER$Exciting.result..O.))[MASTER$Exciting.result..O.], sub = MASTER$T_pval_USE..R.)

sub <- subset(dat, is.na(dat$sub) == FALSE, select = -ncol(dat)) # Select only the replicated studies

### Descriptives
summary(sub)
round(apply(dat, 2, sd, na.rm = TRUE), 4) # Standard deviations

### Variables in the columns
rows <- cbind(MASTER$T_r..O., MASTER$T_pval_USE..O., MASTER$T_N_O_for_tables, MASTER$Institution.prestige..1st.author..O., MASTER$Institution.prestige..senior.author..O.,
              MASTER$Citation.Count..1st.author..O., MASTER$Citation.count..senior.author..O., MASTER$Citation.count..paper..O., MASTER$Internal.conceptual.replications..O.,
              MASTER$Internal.direct.replications..O., as.numeric(levels(MASTER$Surprising.result..O.))[MASTER$Surprising.result..O.], df$sc.impo, MASTER$T_pval_USE..R.)

colnames(rows) <- c("Original.effect.size", "Original.pvalue", "Original.sample.size", "Inst.1st.author", "Inst.sen.author", "Cit.1st.author", "Cit.sen.author",
                    "Cit.impact", "concep.repl", "dir.repl", "surp.res", "impo.res", "sub") 

### "Replications p < 0.05 in original direction (all signicant replication studies are in the same direction as the original study)"
mat <- cbind(rep.dir, rows)
colnames(mat) <- c("rep.dir", "Original.effect.size", "Original.pvalue", "Original.sample.size", "Inst.1st.author", "Inst.sen.author", "Cit.1st.author", "Cit.sen.author",
                   "Cit.impact", "concep.repl", "dir.repl", "surp.res", "impo.res", "sub") 

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "Effect size difference" (Fisher z)
mat <- cbind(es.dif, rows)
colnames(mat) <- c("es.dif", "Original.effect.size", "Original.pvalue", "Original.sample.size", "Inst.1st.author", "Inst.sen.author", "Cit.1st.author", "Cit.sen.author",
                   "Cit.impact", "concep.repl", "dir.repl", "surp.res", "impo.res", "sub") 

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "Meta-analytic estimate" (Fisher z)
mat <- cbind(es.meta.all, rows)
colnames(mat) <- c("Meta-analytic estimate", "Original.effect.size", "Original.pvalue", "Original.sample.size", "Inst.1st.author", "Inst.sen.author", "Cit.1st.author", "Cit.sen.author",
                   "Cit.impact", "concep.repl", "dir.repl", "surp.res", "impo.res", "sub") 

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "original effect size within replication 95% CI"
mat <- cbind(in.ci, rows)
colnames(mat) <- c("Original in CI", "Original.effect.size", "Original.pvalue", "Original.sample.size", "Inst.1st.author", "Inst.sen.author", "Cit.1st.author", "Cit.sen.author",
                   "Cit.impact", "concep.repl", "dir.repl", "surp.res", "impo.res", "sub") 

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

### "subjective 'yes' to 'Did it replicate?'"
mat <- cbind(replic, rows)
colnames(mat) <- c("Did it replicate?", "Original.effect.size", "Original.pvalue", "Original.sample.size", "Inst.1st.author", "Inst.sen.author", "Cit.1st.author", "Cit.sen.author",
                   "Cit.impact", "concep.repl", "dir.repl", "surp.res", "impo.res", "sub") 

sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

round(rcorr(sub, type = "spearman")$r[-1,1], 3) # Create table with correlations and select the appropriate rows

#####################
##### TABLE S4  #####
#####################

### Create data frame
dat <- data.frame(Inst.1st.author = MASTER$Institution.prestige..1st.author..R., Inst.senior.author = MASTER$Institution.prestige..senior.author..R., 
                  Cit.1st.author = MASTER$Citation.count..1st.author..R., Cit.senior.author = MASTER$Citation.count..senior.author..R., position = as.numeric(fac.posi),
                  degree = as.numeric(fac.degr), total.publ = MASTER$Total.publications..R., domain = as.numeric(fac.doma), method = as.numeric(fac.meth), expe.req = as.numeric(fac.expe), 
                  oppo.expec.bias = as.numeric(fac.oppo.expe), oppo.dili = as.numeric(fac.oppo.dili), imple = as.numeric(fac.impl), data.coll = as.numeric(fac.data), 
                  repl.sim = as.numeric(fac.repl), diff.impl = as.numeric(fac.diff), repl.N = MASTER$T_N_R_for_tables, power.R = as.numeric(levels(MASTER$Power..R.))[MASTER$Power..R.], 
                  sur.out = as.numeric(fac.sur.out), sub = MASTER$T_pval_USE..R.)

sub <- subset(dat, is.na(dat$sub) == FALSE, select = -ncol(dat)) # Select only the replicated studies

### Descriptives
summary(sub) 
apply(dat, 2, sd, na.rm = TRUE) # Standard deviations

### Variables in the columns
rows <- cbind(MASTER$Institution.prestige..1st.author..R., MASTER$Institution.prestige..senior.author..R., MASTER$Citation.count..1st.author..R., MASTER$Citation.count..senior.author..R., 
              as.numeric(fac.posi), as.numeric(fac.degr), MASTER$Total.publications..R., as.numeric(fac.doma), as.numeric(fac.meth), as.numeric(fac.expe), as.numeric(fac.oppo.expe), 
              as.numeric(fac.oppo.dili), as.numeric(fac.impl), as.numeric(fac.data), as.numeric(fac.repl), as.numeric(fac.diff), MASTER$T_N_R_for_tables, as.numeric(levels(MASTER$Power..R.))[MASTER$Power..R.], 
              as.numeric(fac.sur.out), MASTER$T_pval_USE..R.)
colnames(rows) <- c("Inst.1st.author", "Inst.senior.author", "Cit.1st.author", "Cit.senior.author", "position", "degree", "total.publ", "domain", "method", "expe.req", "oppo.expec.bias", "oppo.dili",
                    "imple", "data.coll", "repl.sim", "diff.impl", "repl.N", "power.R", "sur.out", "sub")

### "Replications p < 0.05 in original direction (all signicant replication studies are in the same direction as the original study)"
mat <- cbind(rep.dir, rows)
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

rcorr(sub, type = "spearman")$r[-1,1] # Create table with correlations and select the appropriate rows

### "Effect size difference" (Fisher z)
mat <- cbind(es.dif, rows)
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

rcorr(sub, type = "spearman")$r[-1,1] # Create table with correlations and select the appropriate rows

### "Meta-analytic estimate" (Fisher z)
mat <- cbind(es.meta.all, rows)
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

rcorr(sub, type = "spearman")$r[-1,1] # Create table with correlations and select the appropriate rows

### "original effect size within replication 95% CI"
mat <- cbind(in.ci, rows)
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

rcorr(sub, type = "spearman")$r[-1,1] # Create table with correlations and select the appropriate rows

### "subjective 'yes' to 'Did it replicate?'"
mat <- cbind(replic, rows)
sub <- subset(mat, is.na(mat[ , "sub"]) == FALSE, select = -ncol(mat)) # Select only the replicated studies

rcorr(sub, type = "spearman")$r[-1,1] # Create table with correlations and select the appropriate rows
