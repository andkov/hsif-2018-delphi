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
path_input_meta  <- "./data-public/raw/eDelphi-phase2-metadata.csv"
# path_input_meta  <- "./data-public/raw/eDelphi-phase2-metadata-edited.csv"

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

ds0 %>% dplyr::glimpse()
ds_meta %>% dplyr::glimpse()


# ---- extract-live-meta -------------------------
# in order to preserve the original item descriptions, as given by survey results
# let us create a meta data object that would deconstruct survey meta data
# here we will create the meta data to be applied above (retroactively)

# the first two columns contain response id and respondent role
# (stem_cols      <- col_names[1:2] )
# # remaining columns are survey questions
# (survey_columns <- col_names[3:length(col_names)] )
# ls_survey <- list() # empty shell to populate
# for(i in seq_along(survey_columns)){
#   # i <- 2
#   # qname <- paste0("q",i) # q for question
#   subject <- survey_columns[i]
#   
#   regex1 <- "^(.+)Response options include(.+)\\[(.+)?\\]$"
#   ls_survey[[i]] <- list()
#   ls_survey[[i]][["qid"]] <- i
#   ls_survey[[i]][["stem"]]  <- gsub(regex1,"\\1", survey_columns[i])
#   ls_survey[[i]][["scale"]] <- gsub(regex1,"\\2", survey_columns[i])
#   ls_survey[[i]][["item"]]  <- gsub(regex1,"\\3", survey_columns[i])
# }
# ls_meta <- list()
# for(i in seq_along(ls_survey)){
#   ls_meta[[i]] <- ls_survey[[i]] #%>% as.data.frame()
# }
# lapply(ls_meta, names)
# ds_meta_live <- dplyr::bind_rows(ls_meta)
# ds_meta_live %>% dplyr::glimpse()
# ds_meta <- ds_meta %>% dplyr::left_join(ds_meta_live, by = "qid")
# ds_meta %>% dplyr::glimpse()

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
# ds0 %>% dplyr::glimpse() 




# ---- tweak-data --------------------------------------------------------------
# rename the first two columns 
ds1 <- ds0 %>% 
  dplyr::rename_( 
    "response_id"     = "X1",
    "respondent_role" = "X2"
  )
ds1 %>% dplyr::glimpse()

(variables_static  <- c("response_id","respondent_role")) 
(variables_dynamic <- setdiff(names(ds1), variables_static) )

ds2 <- ds1 %>% 
  tidyr::gather_("qid","response_value", variables_dynamic) %>% 
  dplyr::mutate(
    qid = as.integer(gsub("X","",qid)) - 2, # because of the first two variables
    response_value = as.integer(response_value)
  ) %>% 
dplyr::left_join(ds_meta, by = "qid") %>% 
  # dplyr::filter(!is.na(response_value)) %>% 
  dplyr::select(
    response_id, respondent_role, 
    qid, qlabel, response_value, dplyr::everything()
  ) 
ds2 %>% dplyr::glimpse(40)


# ---- inspect-data --------------------
# show groupings
ds_descriptives <- ds2 %>% 
  # dplyr::filter(qid == 1) %>% 
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>% 
  dplyr::group_by(qid, qlabel, respondent_role,target, subject, component) %>%
  # dplyr::group_by(qid, qlabel) %>% 
  dplyr::summarize(
    n_responses = n(),
    mean = mean(response_value, na.rm = T),
    sd   = sd(response_value,na.rm = T)
  )
  


# subset unique questions into a separate data frame
ds2 %>% dplyr::glimpse(50)

  
  

# ---- save-to-disk ----------------------------
readr::write_csv(ds2, "./data-unshared/derived/ds2.csv")
