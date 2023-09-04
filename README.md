# Melanoma-Incidence-in-Norway
R scripts used in the analysis of Melanoma incidence in Norway, 1983–2019

## About R-script files
Most script files depends on two main files `00-CodeMap.R` and `00-Functions.R`. The first one contains all the rules for categorizing continuous variables such as `T-category`, `AgeGroup`, `YearCat` etc. The file also contains the mapping from ICD codes to their category such as `AnatomicSite`, `MelanomaType`. The second file `00-Functions.R` contains all the function created to collect all the common computation. The file also contains a function extensively used to create plot. The Scripts were kept inside a folder `Script` during the analysis so the scripts may contains this path while running these source files.

The code also depends on data to run the complete analysis.
