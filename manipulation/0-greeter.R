# this script imports the raw data described in this shared document 
# https://drive.google.com/file/d/10idMxy8eX8nTHr6wr2Q40x4XOP3Y5ck7/view
# and prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched-output/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes


# ---- declare-globals ---------------------------------------------------------
path_input <- "./data-unshared/raw/eDelphi-phase2results(deidentified).csv"
# path_input <- "./data-unshared/raw/eDelphi-phase2results(deidentified).txt"

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
# for commonly used function see ./manipulation/function-support.R

# ---- load-data ---------------------------------------------------------------
col_names <- readr::read_csv(path_input,n_max = 1, col_names = FALSE) %>% t()
ds0       <- readr::read_csv(path_input,skip = 1,col_names = FALSE) %>% as.data.frame() 

# ---- define-utility-functions ---------------
# function to replace non-UTF-8 characters
replace_with <- function(
  x
  # ,tokens
  # ,replacement
){
  # tokens <- c("<U\\+0092>")
  tokens <- c("u0082")
  replacement <- ""
  for(token in tokens){
    if(is.character(x)){
      x <- gsub(token,replacement,x)
    }
  }
  return(x)
}

# testing and failing
(a <- col_names[61])
# (b <- replace_with(a))
(b <- textclean::mgsub_regex(a, "u0082",""))

# ---- tweak-data --------------------------------------------------------------



respondent_columns <- col_names[1:2]
survey_columns     <- col_names[3:length(col_names)]

# let us create a meta data object
ls_survey <- list()
for(i in seq_along(survey_columns)){
  # i <- 2
  qname <- paste0("q",i)
  subject <- survey_columns[i]

  regex1 <- "^(.+)Response options include(.+)\\[(.+)?\\]$"
  ls_survey[[qname]] <- list()
  ls_survey[[qname]][["stem"]]      <- gsub(regex1,"\\1", survey_columns[i])
  ls_survey[[qname]][["scale"]]     <- gsub(regex1,"\\2", survey_columns[i])
  ls_survey[[qname]][["item"]]  <- gsub(regex1,"\\3", survey_columns[i])
  
}
ls_survey$q50 # q50 has no label for itme

for(i in names(ls_survey)){
  cat("\nItem ",i,"\n")
  print(ls_survey[[i]][["item"]])
}





# ---- save-to-disk ----------------------------

