# Reproducibility Project: Psychology
Repository for working on the Reproducibility Project: Psychology. These files represent the analyses conducted in Tilburg and
reported in the Science publication, as well as the code used to generate Figures 1-3. The Github repository also operates as the issue tracker for the analysis committee.

The data are contained in this repository and were retrieved from rpp_data.csv. Differences might arise due to changes made subsequently to the data. The data used to generate the Figures 1-3 is downloaded from a GoogleSheet.   

## Running the analyses for the RP:P project
There are two ways of getting the files required to reproduce all analyses in the RPP manuscript:

1. Download the zip file `rpp_reproduce.zip` and extract the folder (this is for the non-git users). You can use [this](https://github.com/jlcohoon/rpp/raw/master/rpp_reproduce.zip) link to do that.
2. Clone this git repository and run the `masterscript.R` (this is for the git-users. The command to do this would be `git clone https://github.com/jlcohoon/rpp FOLDERNAME`, where FOLDERNAME is the name of the folder these files will be contained (note your working directory to know where this folder will be placed)

Once the files are downloaded, running the analyses has been made user-friendly (please make sure you have the R statistical package installed, downloadable [here](https://cran.r-project.org/)).

1. Open the `masterscript.R` file in R.
2. Run all
3. Select the directory where you downloaded the files (i.e., the folder where `masterscript.R`, `functions.R`, `RPP_figures.R`, and `rpp_data.csv` are located)
4. Now you can run all the results.

### `masterscript.R` explained
The `masterscript.R` contains all the analyses conducted by the analysis team, save for the additional figures. These have been ordered to correspond with the *Statistical Analyses* section of the manuscript in the Supplemental Information (note there might be some changes in the ordering that were not picked up, but in general it corresponds; if you cannot find something in direct chronological order it usually follows later in that section). 

Note that there was some code run in TURBO PASCAL, which was not run in R and is only available in the supplement.

Lines 39-47 in `masterscript.R` correspond to the *Preliminary analyses* section.

Lines 49-177 in `masterscript.R` correspond to the *Evaluating replication effect against null hypothesis of no effect* section. Note that this section also includes the part with TURBO PASCAL code.

Lines 179-253 in `masterscript.R` correspond to the *Comparing original and replication effect sizes* section.

Lines 255-299 in `masterscript.R` correspond to the figure in *Evaluating replication effect against original effect size* section. Code from [A5] is in lines 533-759

Lines 301-531 are data-ordering code.

Lines 775-920 in `masterscript.R` correspond to the *Combining original and replication effect sizes for cumulative evidence​* section.

Lines 973-1043 in `masterscript.R` correspond to the *Meta­analysis of all original study effects, and of all replication study effects* section.

Lines 922-971 in `masterscript.R` correspond to the *Meta­analysis of difference of effect size between original and replication study* section.

Lines 1101-1446 in `masterscript.R` correspond to the *Moderator Analyses* section.

### Versions of packages used in the analyses
- `R` package: `v3.2.1`
- `metafor` package: `v1.9-7`
- `Hmisc` package: `v3.16-0`   
  
  
******   
******   
  
  
## Generating the Figures in the RP:P project publication  

The `R` code that can be used to generate Figure 1-3 of the RP:P Science article is in the file `RPP_figures.R`   
Download this file either by cloning this repository as explained above, or by simply downloading the linked file (e.g. by right-clicking the name) 

To generate the Figures: 
  
1. Open your local copy of the file `RPP_figures.R` in your `R` environment.  
2. Select `run all` from your `R` GUI or use the `R` console to run: `source('RPP_figures.R', echo=TRUE)` (this assumes the `RPP_figures.R` file is in current working directory)  
  
> **Note:** You will need an internet connection to run the script.  
  
This will create 3 PDF files in the current working directory:  
  
1. `RPP_Figure1_vioQtile.pdf`  - **Figure 1**: Violin Quantile Plots displaying Original and Replication study densities of p-values and effect sizes.  
2. `RPP_Figure2_pvalues.pdf`   - **Figure 2**: Scatterplot of Original and Replication study p-values, with a 'blow-up' of the X-axis.  
3. `RPP_Figure3_ESdensity.pdf` - **Figure 3**: Scatterplot of Original and Replication study Effect sizes, with X and Y margin density plots.  
  
> **Note** Figure appearance will not be exactly the same as in the Science publication.   
  
### `RPP_figures.R` explained 
The `R` source file contains three main sections, *SETUP*, *FIGURE 1*, *FIGURE 2*, *FIGURE 3*.   
   
  
******   
  
   
#### Section: SETUP
All the custom built functions that are required to recreate the Figures are available in a GitHub sourceable file: `C-3PR.R`. It is available [here](https://github.com/FredHasselman/toolboxR), but it is not required (or recommended) to download the file to your local environment.  
  
**Explanation of code in this section:**
   
*Lines 22-23*:  
```{r}
require(devtools)  
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')  
```  
Source `C-3PR.R` directly from GitHub using `source_url()` in package `devtools`.  
  
*Line 26*:
```
in.IT(c('plyr','dplyr','ggplot2','RColorBrewer','scales','lattice','gridExtra'))
``` 
Load (and if necessary install) libraries used for data management and plotting using the `in.IT()` function that is now available due to sourcing `C-3PR`  
   
*Line 30*:
```
RPPdata <- get.GoogleSheet(data='RPPdata',dfCln=TRUE)$df
```
Read the RPP data from a GoogleSheet. Function `get.GoogleSheet()` returns a list with the GoogleSheet data (in field: *df*) and information (in field *info*) containing the URL, a download timestamp, and data objects listing the original column and rownames on the GoogleSheet (these names will be changed to a more user friendly format if `dfCln=TRUE`).  
    
*Lines 32-41*:  
These lines contain code to select the correct studies and prepare some variables needed for plotting.  
   
  
******   
  
   
#### Section: FIGURE 1   
This section contains the code to generate the Violin Quantile Plots. The functions are inspired by the code [Troy](http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot) posted on [Stack Overflow](http://stackoverflow.com) on 10-03-2014. Thank you Troy!   
    
**Explanation of code in this section:**   
   
*Lines 48-55*:  
This code creates a temporary dataset convenient for plotting the violin plots (i.e. long format data with `Study` as a factor indicating an original or replication study).
   
*Lines 56-82*:  
This code plots panel A of the figure containing the p-values.    
First the quantiles are caclulated from the data and output to the `R` console:   
```
> ori
                freq
[0,0.0004]        24
(0.0004,0.0069]   26
(0.0069,0.0232]   22
(0.0232,0.912]    26
  
> rep
               freq
[0,0.0077]       24
(0.0077,0.198]   26
(0.198,0.534]    24
(0.534,0.975]    24
```
   
Then the plot is created as follows:  
```
# Get regular violinplot using package ggplot2
g.pv <- ggplot(df,aes(x=grp,y=p.value)) + 
        geom_violin(aes(group=grp),scale="width",color="grey30",fill="grey30",trim=T,adjust=.7)
# Cut at quantiles using vioQtile() in C-3PR
g.pv0 <- vioQtile(g.pv,qtiles,probs)
# Garnish 
g.pv1 <- g.pv0 + geom_hline(aes(yintercept=.05),linetype=2) + 
         ggtitle("A") + xlab("") + ylab("p-value") + 
         mytheme
# View
g.pv1

## Uncomment to save panel A as a seperate file
# ggsave("RPP_F1_VQPpv.eps",plot=g.pv1)
```
  
  
*Lines 83-109*:  
This code plots panel B of the figure containing the effect sizes.  
First the quantiles are caclulated from the data and output to the `R` console:   
```
> ori
                freq
[0.00464,0.222]   24
(0.222,0.372]     25
(0.372,0.551]     25
(0.551,0.86]      24
   
> rep
               freq
[-0.45,0.0194]   24
(0.0194,0.12]    24
(0.12,0.323]     25
(0.323,0.923]    24
```
   
Then the plot is created as follows:  
```
# Get regular violinplot using package ggplot2
g.es  <- ggplot(df,aes(x=grp,y=EffectSize)) + 
         geom_violin(aes(group=grpN),scale="width",fill="grey40",color="grey40",trim=T,adjust=1) 
# Cut at quantiles using vioQtile() in C-3PR
g.es0 <- vioQtile(g.es,qtiles=qtiles,probs=probs) 
# Garnish
g.es1 <- g.es0 + 
         ggtitle("B") + xlab("") + ylab("Effect Size") + 
         scale_y_continuous(breaks=c(-.25,-.5,0,.25,.5,.75,1),limits=c(-.5,1)) + mytheme 
# View
g.es1 

# # Uncomment to save panel B as a seperate file
# ggsave("RPP_F1_VQPes.eps",plot=g.es1)
```
   
*Lines 110-118*:  
To view and save the combined plot the function `multi.PLOT()` is used. This function was copied from the online version of [Winston Chang's Cookbook for R](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/). Thanks Winston!   
```
# VIEW panels in one plot using the multi.PLOT() function from C-3PR
multi.PLOT(g.pv1,g.es1,cols=2)  

# SAVE combined plots as PDF
pdf("RPP_Figure1_vioQtile.pdf",pagecentre=T, width=20,height=8 ,paper = "special")
multi.PLOT(g.pv1,g.es1,cols=2)
dev.off()
```   
    
  
******   
  
   
#### Section: FIGURE 2   
This section contains the code to generate the scatterplot comparing Original and Replication study p-values. The Figure contains a subplot which is a magnification of the scale of the X-axis. The grey lines in the published figure were added using image editing software.   
   
  
**Explanation of code in this section:**   
   
*Lines 130-145*:  
This code sets up the viewport such that the main scatterplot and the magnification are plotted correctly. 
```
# One of many ways to manipulate positions of multiple plots in one output device
vpM <- viewport(width = 0.6, height = 0.6, x=0.4, y=.7)
vpZ <- viewport(width = 0.6, height = 0.3, x=.4, y=.25)
grid.show.viewport(vpM)
grid.show.viewport(vpZ)
# This will position a ggplot "main" and "sub" in their respective slots
full <- function() {
     plot.new()
     print(main,vp = vpM)
     print(sub, vp = vpZ)
 }
```
If the function `full()` is called, an object `main` and `sub` will be arranged on the viewport according to information provided in `vpM` and `vpZ`. The command `grid.show.viewport()` of package `grid` will reveal the coordinates defined int hese variables. 
   
   
*Lines 146-171*: 
This code creates two ggplot objects, `main` and `sub`.  
```
main <- ggplot(RPPdata,aes(x=T.pval.USE.O,y=T.pval.USE.R))+ 
  scale_x_continuous(breaks=c(0,.01,.05),limits=c(0,.06)) + 
  ggtitle("") + xlab("Original Study p-value") + ylab("Replication p-value") + 
  scale_color_brewer(name="Journal",palette="Set2") + 
  scale_size_continuous(name="Replication:\nPower",breaks=seq(0,1,length=11))  + ylim(c(0,1)) + 
  geom_vline(aes(xintercept=0.05),linetype=2,color=mypalette[9]) + 
  geom_hline(aes(yintercept=0.05),linetype=2,color=mypalette[9]) + 
  geom_point(aes(size=Power.Rn,color=Journal.O),alpha=.8) + mytheme

## Uncomment to save subplot
# ggsave("RPP_F2_pvMain.png",plot=main)

sub <- ggplot(RPPdata,aes(x=T.pval.USE.O,y=T.pval.USE.R)) + 
  geom_vline(aes(xintercept=0.05),linetype=2,color=mypalette[9]) +
  geom_hline(aes(yintercept=0.05),linetype=2,color=mypalette[9]) + 
  geom_point(aes(size=Power.Rn,color=Journal.O),alpha=.8) + 
  scale_x_continuous(breaks=c(0,.001,.005),limits=c(0,.005)) + 
  ggtitle("0 < p < 0.005") + 
  xlab("")+ ylab("") + 
  scale_size_continuous(guide=F) +
  scale_color_brewer(palette="Set2",guide=F) +
  ylim(c(0,1)) + mytheme

## Uncomment to save subplot
# ggsave("RPP_F2_pvsub.png",plot=sub)
```
   
*Lines 172-179*:   
To view and save the combined plot call the function `full()` defined above.
```
# VIEW combined plots
full()
   
# SAVE combined plots as PDF (note: the lines emphasizing the 'blow-up' figure in the publication were drawn in later using image editing software)
pdf("RPP_Figure2_pvalues.pdf",pagecentre=T, width=15,height=8 ,paper = "special")
full()
dev.off()
```  
    
  
******   
  
   
#### Section: FIGURE 3   
This section contains the code to generate the scatterplot comparing Original and Replication study effect sizes. The Figure margins contain density plots of the X and Y axis.  
   
    
**Explanation of code in this section:**   
   
*Lines 187-202*:  
This code sets up some variables needed to create the plots.  
The function plotHolder() is used to create a 'dummy' plot needed to create the margin plot.    
   
*Lines 203-243*:  
This code creates the marginplots `xDense` and `yDense` and the main scatterplot `scatterP`.
```
# X margin density plot (note: gg.theme() from C-3PR can be used directly in a ggplot2() call)
xDense <- ggplot(RPPdata, aes(x=T.r.O, fill=oriSig)) + 
  geom_density(aes(y= ..count..),trim=F,alpha=.5) + 
  xlab("") + ylab("") + xlim(0,1) +
  gg.theme("noax") + 
  theme(legend.position = "none",plot.margin = unit(c(0,0,0,4), "lines"))

## Uncomment to save subplot
# ggsave("RPP_F3_xDense.png",plot=xDense)

# Y margin density plot (note: gg.theme() from C-3PR can be used directly in a ggplot2() call)
yDense <- ggplot(RPPdata, aes(x=T.r.R, fill=repSig)) + 
  geom_density(aes(y= ..count..),trim=F,alpha=.5) + 
  xlab("") + ylab("") + xlim(-.5,1) + 
  coord_flip() + 
  gg.theme("noax") + 
  theme(legend.position = "none", plot.margin = unit(c(0,0,3,0), "lines")) 

## Uncomment to save subplot
# ggsave("RPP_F3_yDense.png",plot=yDense)

# The main scatterplot (note: gg.theme() from C-3PR can be used directly in a ggplot2() call)
scatterP<-
  ggplot(RPPdata,aes(x=T.r.O,y=T.r.R)) +  
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_abline(intercept=0,slope=1,color="Grey60")+
  geom_point(aes(size=Power.Rn,fill=repSig),color="Grey30",shape=21,alpha=.8) + 
  geom_rug(aes(color=oriSig),size=1,sides="b",alpha=.6) + 
  geom_rug(aes(color=repSig),,size=1,sides="l",alpha=.6) + 
  scale_x_continuous(name="Original Effect Size",limits=c(0,1),breaks=c(0,.25,.5,.75,1)) + 
  scale_y_continuous(name="Replication Effect Size",limits=c(-.5,1),breaks=c(-.5,-.25,0,.25,.5,.75,1)) + 
  ggtitle("") + xlab("") + ylab("") + 
  scale_size_continuous(name="Replication Power",range=c(2,9)) + 
  scale_color_discrete(name="p-value") +
  scale_fill_discrete(name="p-value") +
  gg.theme("clean") + 
  theme(legend.position=c(.9,.6), plot.margin = unit(c(-2,-1.5,2,2), "lines")) 

## Uncomment to save subplot
# ggsave("RPP_F3_scatter.png",plot=scatterP)
```
   
*Lines 244-250*: 
To view and save the combined plots the function `grid.arrange()` from the `gridExtra` package is used.
```
grid.arrange(xDense, blankPlot, scatterP, yDense, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

# SAVE combined plots as PDF
pdf("RPP_Figure3_ESdensity.pdf",pagecentre=T, width=15,height=12 ,paper = "special")
grid.arrange(xDense, blankPlot, scatterP, yDense, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
dev.off()
```
  
### Versions of packages used to generate the figures (not showing imports)
- `R` version: `3.1.3 (2015-03-09)`
- `devtools` version: `1.8.0`
- `dplyr` version: `0.4.2`
- `ggplot2` version: `1.0.1`
- `gridExtra` version: `2.0.0`
- `lattice` version: `0.20-33`
- `httr` version: `1.0.0`
- `plyr` version: `1.8.3`
- `RColorBrewer` version: `1.1-2`

  

