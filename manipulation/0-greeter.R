# this script imports the raw data collected during quatitative phase 2 of Delphi study
# this script prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

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
path_input_micro <- "./data-unshared/raw/eDelphi-phase2.csv"
path_input_meta  <- "./data-unshared/raw/eDelphi-phase2-metadata.csv"

# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input_micro))
testit::assert("File does not exist", base::file.exists(path_input_meta))

# declare where you will store the product of this script
path_save <- "./data-unshared/derived/0-greeted.rds"

# See definitions of commonly  used objects in:
source("./manipulation/object-glossary.R")   # object definitions

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
# for commonly used function see ./manipulation/function-support.R

# ---- load-data ---------------------------------------------------------------
# column names are very long, isolate to a separate object
col_names <- readr::read_csv(path_input_micro,n_max = 1, col_names = FALSE) %>% t()
# the main body of the survey, all the questions
ds0       <- readr::read_csv(path_input_micro,skip = 1,col_names = FALSE) %>% as.data.frame() 
# manually created object ()
ds_meta   <- readr::read_csv(path_input_meta)

# ---- tweak-data --------------------------------------------------------------
stem_cols <- col_names[1:2]
survey_columns     <- col_names[3:length(col_names)]

# let us create a meta data object
ls_survey <- list()
for(i in seq_along(survey_columns)){
  # i <- 2
  qname <- paste0("q",i)
  subject <- survey_columns[i]
  
  regex1 <- "^(.+)Response options include(.+)\\[(.+)?\\]$"
  ls_survey[[qname]] <- list()
  ls_survey[[qname]][["qname"]] <- qname
  ls_survey[[qname]][["stem"]]  <- gsub(regex1,"\\1", survey_columns[i])
  ls_survey[[qname]][["scale"]] <- gsub(regex1,"\\2", survey_columns[i])
  ls_survey[[qname]][["item"]]  <- gsub(regex1,"\\3", survey_columns[i])
}
ls_meta <- list()
for(i in names(ls_survey)){
  ls_meta[[i]] <- ls_survey[[i]] #%>% as.data.frame()
}
ds_meta_live <- dplyr::bind_rows(ls_meta)
ds_meta <- ds_meta %>% dplyr::left_join(ds_meta_live, by = "qname")

# # extract the items questions for easier handling
# ls_new <- list()
# for(i in names(ls_survey)){
#   ls_new[[i]] <- ls_survey[[i]][["item"]] %>% as.data.frame()
# }
# ds_new <- dplyr::bind_rows(ls_new)
# # write down so you can pick it up in excel
# readr::write_csv(ds_new,"./data-unshared/derived/q_names.csv")
# # next, add meta to "./data-unshared/raw/eDelphi-phase2-metadata.csv"
# # use the excel spread sheet of the same name to make pretty

variables_static  <- c("response_id","respondent_role")
variables_dynamic <- names(ls_survey)
names(ds0) <- c( variables_static, variables_dynamic )

ds1_long <- ds0 %>% 
  tidyr::gather_("qname","value", variables_dynamic) %>% 
  dplyr::left_join(ds_meta, by = "qname")


# ---- save-to-disk ----------------------------
readr::write_csv(ds1_long, "./data-unshared/derived/ds1_long.csv")
