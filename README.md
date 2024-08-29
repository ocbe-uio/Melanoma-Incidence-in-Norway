# Melanoma-Incidence-in-Norway
R scripts used in the analysis of Melanoma incidence in Norway, 1983–2019

## About R-script files
Most script files depend on two main files `00-CodeMap.R` and `00-Functions.R`. The first one contains all the rules for categorizing continuous variables such as `T-category`, `AgeGroup`, `YearCat` etc. The file also includes the mapping from ICD codes to their category such as `AnatomicSite`, and `MelanomaType`. The second file `00-Functions.R` contains all the functions created to collect all the common computations. The file also includes a function extensively used to develop plots used in the paper. The Scripts were kept inside different folders during the analysis so the scripts may contain this path while running these source files.

The code also depends on data and the correct path for data and script to run the complete analysis.
