# Reproducibility project
Repository for working on the Reproducibility project. Here we develop the analyses for Tilburg. The Github repository also operates as the issue tracker for the Tilburg people on the analysis committee.

The data are contained in this repository and were retrieved from the [Masterdata](https://docs.google.com/spreadsheets/d/10IXGYUvt9vb64FyXP2Wlf03X5lPo_AvhQOsNs6w84dk/edit#gid=0) sheet on March 23, 2015. Differences might arise due to changes made subsequently to the Masterdata.

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

Lines ??-?? in `masterscript.R` correspond to the *Combining original and replication effect sizes for cumulative evidence​* section.

Lines ??-?? in `masterscript.R` correspond to the *Subjective assessment of “Did it replicate?”* section.

Lines ??-?? in `masterscript.R` correspond to the *Meta­analysis of all original study effects, and of all replication study effects* section.

Lines ??-?? in `masterscript.R` correspond to the *Meta­analysis of difference of effect size between original and replication study* section.

Lines ??-?? in `masterscript.R` correspond to the *Moderator Analyses* section.

