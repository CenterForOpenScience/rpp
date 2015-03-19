
######################################################################################
# Master script Tilburg University Analyses                                          #
# Based on Google Spreadsheet for ultimate reproducibility.                          #  
# Responsible for this file: CHJ Hartgerink (c.h.j.hartgerink@tilburguniversity.edu) #
# This script file is self-contained. All used functions are provided here           #
######################################################################################

library(devtools)
source_url('https://gist.githubusercontent.com/chartgerink/cdf80f331bcde3d3103a/raw/aaaf193562ae04fc9983a97ff7ee2748846a04ef/RPPfunctions')

# Read in the google spreadsheet
# Written by Sacha Epskamp
library("httr")
url <- "https://docs.google.com/spreadsheet/ccc?key=10IXGYUvt9vb64FyXP2Wlf03X5lPo_AvhQOsNs6w84dk&single=true&gid=0&output=csv"
GET(url, write_disk("MASTER.csv", overwrite = TRUE))
MASTER <- read.csv("MASTER.csv")
####
###########
#TO DELETE# 
###########
## Recalculating p-values
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
