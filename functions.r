# Color transparency function written by Sacha Epskamp
# Retrieved from http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r

# usage: col = addTrans("col", 255) where 0 is fully transparent and 255 non-transparent.

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

###
# Effect size computation
# Written by CHJ Hartgerink
esComp <- function(
  x,
  df1,
  df2,
  N,
  esType){
  esComp <- ifelse(esType=="t",
                   sqrt((x^2*(1 / df2)) / (((x^2*1) / df2) + 1)),
                   ifelse(
                     esType=="F",
                     sqrt((x*(df1 / df2)) / (((x*df1) / df2) + 1))*sqrt(1/df1),
                     ifelse(
                       esType=="r",
                       x,
                       ifelse(
                         esType=="Chi2",
                         sqrt(x/N),
                         ifelse(
                           esType == "z",
                           tanh(x * sqrt(1/(N-3))), 
                           NA
                         )
                       )
                     )
                   ))
  
  return(esComp)
}

###
# Written by CHJ Hartgerink
# The Fisher method applied to test for deviation from uniformity
# In NONSIGNIFICANT P-values

FisherMethod <- function(# Compute Fisher's exact test for non-significant p-values.
  ### This function computes paper level Fisher test statistics, testing whether the distribution of non-significant p-values is uniform. Significant values indicate deviation from uniformity. 
  ### Returns both the normal Fisher test, as well as the complement test.
  ### Computations are done for p*=log(p), where p is all non-significant p-values for each identifier.
  x,
  ### Vector of p-values.
  id,
  ### Vector giving paper identifiers.
  alpha = .05
  ### Indicate what alpha level is being maintained for the study results, which serves as a cut-off for selecting the non-significant p-values.
){
  Res <- NULL
  for(i in 1:length(unique(id)))
  {
    selP <- x[id==unique(id)[i]]
    nSigP <- (na.omit(selP[selP>alpha])-alpha)/(1-alpha)
    SigP <- na.omit(selP[selP<=alpha])
    if(!length(nSigP)==0){
      # Compute the Fisher test statistic
      FMeth <- -2*sum(log(nSigP))
      pFMeth <- pchisq(q=FMeth, df=2*length(nSigP), lower.tail=F)
    } else {
      FMeth <- NA
      pFMeth <- NA
    }
    Res <- rbind(Res, data.frame(
      Fish = FMeth,
      PFish = pFMeth,
      CountNSig = length(nSigP),
      CountSig = length(SigP),
      PercentNonSig = length(nSigP)/length(selP)))
  }
  return(Res)
}

###
# Recalculating p-values
# Written by CHJ Hartgerink and RCM van Aert

pvalr <- function(x, N) {
  fis.r <- 0.5*log((1 + x) / (1 - x))
  se.fis.r <- sqrt(1/(N-3))
  pnorm(fis.r, mean = 0, sd = se.fis.r, lower.tail = FALSE)
}

# Computes two-tailed p-value
pvalComp <- function(
  x,
  df1,
  df2,
  N,
  esType){
  pvalComp <- ifelse(esType=="t",
                     pt(abs(x), df = df2, lower.tail = FALSE) * 2,
                     ifelse(
                       esType=="F",
                       pf(x, df1 = df1, df2 = df2, lower.tail = FALSE),
                       ifelse(
                         esType=="r",
                         pvalr(abs(x), N) * 2,
                         ifelse(
                           esType=="Chi2",
                           pchisq(x, df = df1, lower.tail = FALSE),
                           ifelse(
                             esType == "z",
                             pnorm(abs(x), lower.tail = FALSE) * 2, 
                             NA
                           )
                         )
                       )
                     ))
  
  return(pvalComp)
}