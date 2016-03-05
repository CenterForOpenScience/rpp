# Load packages
library(devtools)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')
# This will load and (if necessary) install libraries frequently used for data management and plotting
in.IT(c('plyr','dplyr','httr','rio','haven'))

# Read the data from OSF storage
#
# Note: get.OSFfile() returns a list with the .csv data (df) and information (info) containing the URL download timestamp and original column and rownames (these names will be changed if dfCln=TRUE).
RPPdata <- get.OSFfile(code='https://osf.io/fgjvw/',dfCln=TRUE)$df

# Figure out how many NAs result from changing to numeric
errs <- llply(1:ncol(RPPdata), function(c) try.CATCH(sum(is.na(as.numeric(RPPdata[c][[1]])))))
nNA  <- laply(seq_along(errs), function(e) if(is.integer(errs[[e]]$value)){errs[[e]]$value}else{168})
# Which variables are numeric
nINT <- colwise(is.numeric)(RPPdata)

# These variables are likely Character format and should be changed to Numeric format
varnames <-  colnames(RPPdata[,(nNA<168)&!nINT])
# "Surprising.result.O"          "Exciting.result.O"            "Replicated.study.number.R"    "N.O"
# "Reported.P.value.O"           "X.Tails.O"                    "X80.power"                    "X90.power"
# "X95.power"                    "Planned.Power"                "Original.Author.s.Assessment" "P.value.R"
# "Power.R"

colnames(RPPdata)

# Fix it!
for(cn in varnames){
    RPPdata[cn] <- as.numeric(RPPdata[cn][[1]])
}

# Use package rio for export to:
#
# Comma seperated
rio::export(RPPdata,"RPPdataConverted.csv")
# Tab delimeted
rio::export(RPPdata,"RPPdataConverted.tsv")
# SPSS
haven::write_sav(RPPdata,"RPPdataConverted.sav")
# Excel
rio::export(RPPdata,"RPPdataConverted.xlsx")
# Stata
rio::export(RPPdata,"RPPdataConverted.dta")
# R (serialized object)
rio::export(RPPdata,"RPPdataConverted.rds")
