# Load packages
library(devtools)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')
# This will load and (if necessary) install libraries frequently used for data management and plotting
in.IT(c('plyr','dplyr','httr','rio'))

# Read the data from OSF storage
#
# Note: get.OSFfile() returns a list with the .csv data (df) and information (info) containing the URL download timestamp and original column and rownames (these names will be changed if dfCln=TRUE).
RPPdata <- get.OSFfile(code='https://osf.io/fgjvw/',dfCln=TRUE)$df

# Use package rio for export to:
#
# Comma seperated
export(RPPdata,"RPPdataConverted.csv")
# Tab delimeted
export(RPPdata,"RPPdataConverted.tsv")
# SPSS
export(RPPdata,"RPPdataConverted.sav")
# Excel
export(RPPdata,"RPPdataConverted.xlsx")
# Stata
export(RPPdata,"RPPdataConverted.dta")
# R (serialized object)
export(RPPdata,"RPPdataConverted.rds")
