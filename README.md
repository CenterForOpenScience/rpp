# Reproducibility project
Repository for working on the Reproducibility project. Here we develop the analyses for Tilburg. The Github repository also operates as the issue tracker for the Tilburg people on the analysis committee.

The data are contained in this repository and were retrieved from the [Masterdata](https://docs.google.com/spreadsheets/d/10IXGYUvt9vb64FyXP2Wlf03X5lPo_AvhQOsNs6w84dk/edit#gid=0) sheet on March 23, 2015. Differences might arise due to changes made subsequently to the Masterdata.

## Running the analyses for the RP:P project
There are two ways of getting the files required to reproduce all analyses in the RPP manuscript:

1. Download the zip file `rpp_reproduce.zip` and extract the folder (this is for the non-git users). You can use [this]() link to do that.
2. Clone this git repository and run the `masterscript.R` (this is for the git-users. The command to do this would be `git clone https://github.com/jlcohoon/rpp FOLDERNAME`, where FOLDERNAME is the name of the folder these files will be contained (note your working directory to know where this folder will be placed)

Once the files are downloaded, running the analyses has been made user-friendly (please make sure you have the R statistical package installed, downloadable [here](https://cran.r-project.org/)).

1. Open the `masterscript.R` file in R.
2. Run all
3. Select the directory where you downloaded the files (i.e., the folder where `masterscript.R`, `functions.R`, `RPP_figures.R`, and `rpp_data.csv` are located)
4. Now you can run all the results.



