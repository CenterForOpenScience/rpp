
######################################################################################
# Master script Tilburg University Analyses                                          #
# Based on Google Spreadsheet for ultimate reproducibility.                          #  
# Responsible for this file: CHJ Hartgerink (c.h.j.hartgerink@tilburguniversity.edu) #
# This script file is self-contained. All used functions are provided here           #
######################################################################################

library(devtools)
source_url('https://raw.githubusercontent.com/chartgerink/rpp/master/functions.r')

# Read in the google spreadsheet
# Written by Sacha Epskamp
library("httr")
url <- "https://docs.google.com/spreadsheet/ccc?key=10IXGYUvt9vb64FyXP2Wlf03X5lPo_AvhQOsNs6w84dk&single=true&gid=0&output=csv"
GET(url, write_disk("MASTER.csv", overwrite = TRUE))
MASTER <- read.csv("MASTER.csv")
####

############
#TO DELETE?# 
############
# Recalculating p-values
MASTER$T_pval.recalc..O. <- pvalComp(x = MASTER$T_Test.value..O.,
                                     df1 = MASTER$T_df1..O.,
                                     df2 = MASTER$T_df2..O.,
                                     N = MASTER$T_N..O.,
                                     esType = MASTER$T_Test.Statistic..O.)
writeClipboard(as.character(MASTER$T_pval.recalc..O.))

MASTER$T_pval.recalc..R. <- pvalComp(x = MASTER$T_Test.value..R.,
                                     df1 = MASTER$T_df1..R.,
                                     df2 = MASTER$T_df2..R.,
                                     N = MASTER$T_N..R.,
                                     esType = MASTER$T_Test.Statistic..R.)
writeClipboard(as.character(MASTER$T_pval.recalc..R.))

# Computing effects
MASTER$T_r..O. <- esComp(x = MASTER$T_Test.value..O.,
                         df1 = MASTER$T_df1..O.,
                         df2 = MASTER$T_df2..O.,
                         N = MASTER$T_N..O.,
                         esType = MASTER$T_Test.Statistic..O.)
writeClipboard(as.character(MASTER$T_r..O.))

MASTER$T_r..R. <- esComp(x = MASTER$T_Test.value..R.,
                         df1 = MASTER$T_df1..R.,
                         df2 = MASTER$T_df2..R.,
                         N = MASTER$T_N..R.,
                         esType = MASTER$T_Test.Statistic..R.)
writeClipboard(as.character(MASTER$T_r..R.))



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

###########
# Overall #
###########
sel <- MASTER[!is.na(MASTER$T_r..O.) & !is.na(MASTER$T_r..R.),]

# Column B
# Relations tested
reltest <- dim(sel)[1]
# Relations both significant
relsig <- sum((sel$T_pval_USE..O.) <= .05 & (sel$T_pval_USE..R. <= .05), na.rm = TRUE)
writeClipboard(
  paste0(
    round(relsig, 0),
    "/",
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

###############
# Per journal #
###############

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
      "/",
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

###########
# P-value analyses
# Written by CHJ Hartgerink


###

# Effect size distributions
# Written by CHJ Hartgerink

###

# Meta-analysis
# Written by RCM van Aert

###

# Moderator analysis
# Written by RCM van Aert
