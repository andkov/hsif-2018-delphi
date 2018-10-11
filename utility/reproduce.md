



This report was automatically generated with the R package **knitr**
(version 1.20).


```r
# knitr::stitch_rmd(script="./utility/reproduce.R", output="./utility/reproduce.md")

# Reproducible Research ---------------------------------------------------
#' When executed by R, this file will manipulate the original data sources (ie, ZZZZ)
#' to produce a groomed dataset suitable for analysis and graphing.

# Clear memory from previous runs -----------------------------------------
base::rm(list=base::ls(all=TRUE))

# Check Working Directory -------------------------------------------------
#' Verify the working directory has been set correctly.  Much of the code assumes the working directory is the repository's root directory.
#' In the following line, rename `RAnalysisSkeleton` to your repository.
if( base::basename(base::getwd()) != "RAnalysisSkeleton" ) {
  base::stop("The working directory should be set to the root of the package/repository.  ",
       "It's currently set to `", base::getwd(), "`.")
}
```

```
## Error in eval(expr, envir, enclos): The working directory should be set to the root of the package/repository.  It's currently set to `C:/Users/koval/Documents/GitHub/andkov/hsif-2018-delphi`.
```

```r
# Install the necessary packages ------------------------------------------
path_install_packages <- "./utility/install-packages.R"
if( !file.exists(path_install_packages)) {
  base::stop("The file `", path_install_packages, "` was not found.  Make sure the working directory is set to the root of the repository.")
}
base::source(path_install_packages, local=new.env())
```

```
## Loading required namespace: devtools
```

```
## Downloading GitHub repo OuhscBbmc/OuhscMunge@master
## from URL https://api.github.com/repos/OuhscBbmc/OuhscMunge/zipball/master
```

```
## Installing OuhscMunge
```

```
## Installing 1 package: DBI
```

```
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'DBI' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: devtools
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'devtools' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: digest
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## 
##   There is a binary version available (and will be installed) but
##   the source version is later:
##        binary source
## digest 0.6.17 0.6.18
## 
## package 'digest' successfully unpacked and MD5 sums checked
```

```
## Warning: cannot remove prior installation of package 'digest'
```

```
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: glue
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'glue' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: lubridate
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'lubridate' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: odbc
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'prettyunits', 'bit', 'blob', 'bit64'
```

```
## package 'prettyunits' successfully unpacked and MD5 sums checked
## package 'bit' successfully unpacked and MD5 sums checked
## package 'blob' successfully unpacked and MD5 sums checked
## package 'bit64' successfully unpacked and MD5 sums checked
## package 'odbc' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: purrr
```

```
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'purrr' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: remotes
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'remotes' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: rlang
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'rlang' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## Installing 1 package: testit
## Installing package into 'C:/Users/koval/Documents/R/win-library/3.4'
## (as 'lib' is unspecified)
```

```
## package 'testit' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\koval\AppData\Local\Temp\RtmpMtSXdD\downloaded_packages
```

```
## "C:/PROGRA~1/R/R-34~1.4/bin/x64/R" --no-site-file --no-environ --no-save  \
##   --no-restore --quiet CMD INSTALL  \
##   "C:/Users/koval/AppData/Local/Temp/RtmpMtSXdD/devtools13f843e042e0/OuhscBbmc-OuhscMunge-b614fcf"  \
##   --library="C:/Users/koval/Documents/R/win-library/3.4" --install-tests
```

```
## 
```

```
## Installation failed: Command failed (1)
```

```
## Error in loadNamespace(name): there is no package called 'OuhscMunge'
```

```r
base::rm(path_install_packages)

# Load the necessary packages ---------------------------------------------
base::requireNamespace("base")
base::requireNamespace("knitr")
base::requireNamespace("markdown")
```

```
## Loading required namespace: markdown
```

```r
base::requireNamespace("testit")
```

```
## Loading required namespace: testit
```

```r
######################################################################################################
#' The following example comes from https://github.com/wibeasley/Wats.  Rename the paths appropriately.

# Declare the paths of the necessary files --------------------------------

#' The raw/input data files:
# pathCensus199x <- base::paste0("./Datasets/CensusIntercensal/STCH-icen199", 0:9, ".txt")
# pathCensus200x <- "./Datasets/CensusIntercensal/CO-EST00INT-AGESEX-5YR.csv"
# pathCountyFips <- "./Datasets/CountyFipsCode.csv"

#' The derived/intermediate data files (which are produced by the repository's code files):
# pathCensusYearly <- "./Datasets/CensusIntercensal/CensusCountyYear.csv"
# pathCensusMonthly <- "./Datasets/CensusIntercensal/CensusCountyMonth.csv"
# pathDataForAnalaysis2005 <- "./Datasets/CountyMonthBirthRate2005Version.csv"
# pathDataForAnalaysis2014 <- "./Datasets/CountyMonthBirthRate2014Version.csv"

#' Code Files:
# pathManipulateCensus <- "./UtilityScripts/IsolateCensusPopsForGfr.R"
# pathCalculateGfr <- "./UtilityScripts/CalculateGfr.R"

#' Report Files:
# pathsReports <- base::file.path("./vignettes", c("MbrFigures.Rmd", "OkFertilityWithIntercensalEstimates.Rmd"))

# Verify the necessary path can be found ----------------------------------

#' The raw/input data files:
# testit::assert("The 10 census files from 199x should exist.", base::file.exists(pathCensus199x))
# testit::assert("The 200x census file should exist.", base::file.exists(pathCensus200x))
# testit::assert("The county FIPS values should exist.", base::file.exists(pathCountyFips))

#' Code Files:
# testit::assert("The file that restructures the census data should exist.", base::file.exists(pathManipulateCensus))
# testit::assert("The file that calculates the GFR should exist.", base::file.exists(pathCalculateGfr))

# Report Files:
# testit::assert("The knitr Rmd files should exist.", base::file.exists(pathsReports))

# Run the files that manipulate and analyze -------------------------------

#' Execute code that restructures the Census data
# base::source(pathManipulateCensus, local=base::new.env())

#' Assert that the intermediate files exist (the two files produced by `IsolateCensusPopsForGfr.R`)
# testit::assert("The yearly records should exist.", base::file.exists(pathCensusYearly))
# testit::assert("The monthly records should exist.", base::file.exists(pathCensusMonthly))

#' Execute code that combines the census and birth count data.
# base::source(pathCalculateGfr, local=base::new.env())

#' Verify that the two human readable datasets are present.
# testit::assert("The CSV for the 2005 Version should exist.", base::file.exists(pathDataForAnalaysis2005))
# testit::assert("The CSV for the 2014 Version should exist.", base::file.exists(pathDataForAnalaysis2014))

# Build the reports -------------------------------------------------------
# for( pathRmd in pathsReports ) {
#   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
#   pathHtml <- base::gsub(pattern=".Rmd$", replacement=".html", x=pathRmd)
#   knitr::knit(input=pathRmd, output=pathMd)
#   markdown::markdownToHTML(file=pathMd, output=pathHtml)
# }
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## Warning in FUN(X[[i]], ...): DESCRIPTION file of package 'digest' is
## missing or broken
```

```
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows >= 8 x64 (build 9200)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
```

```
## Error in x[["Version"]]: subscript out of bounds
```

```r
Sys.time()
```

```
## [1] "2018-10-11 09:49:11 PDT"
```

