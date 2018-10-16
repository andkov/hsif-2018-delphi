# knitr::stitch_rmd(script="./analysis/report-1/report-1.R", output="./analysis/report-1/report-1.md")
# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./analysis/report-2/report-2.R",
#   output = "./analysis/report-2/report-2.md"
# )
# this command is typically executed by the ./manipulation/governor.R


rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")
base::source("./scripts/graphing/graph-presets.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
requireNamespace("dplyr")
# requireNamespace("tidyr") #For converting wide to long
# requireNamespace("RColorBrewer")
# requireNamespace("scales") #For formating values in graphs
# requireNamespace("mgcv) #For the Generalized Additive Model that smooths the longitudinal graphs.
# requireNamespace("TabularManifest") # devtools::install_github("Melinae/TabularManifest")

# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

path_input <- "./data-unshared/derived/ds2.csv"

# ---- function-copy -------------------------
# The two graphing functions are copied from https://github.com/Melinae/TabularManifest.
# histogram_discrete <- function(
#   d_observed,
#   variable_name,
#   levels_to_exclude   = character(0),
#   main_title          = variable_name,
#   x_title             = NULL,
#   y_title             = "Number of Included Records",
#   text_size_percentage= 6,
#   bin_width           = 1L,
#   font_base_size      = 12
# ) {
# 
#   # Ungroup, in case it comes in grouped.
#   d_observed <-
#     d_observed %>%
#     dplyr::ungroup()
# 
#   if( !base::is.factor(d_observed[[variable_name]]) )
#     d_observed[[variable_name]] <- base::factor(d_observed[[variable_name]])
# 
#   d_observed$iv <- base::ordered(d_observed[[variable_name]], levels=rev(levels(d_observed[[variable_name]])))
# 
#   d_count <- dplyr::count_(d_observed, vars ="iv" )
#   # if( base::length(levels_to_exclude)>0 ) { }
#   d_count <- d_count[!(d_count$iv %in% levels_to_exclude), ]
# 
#   d_summary <- d_count %>%
#     dplyr::rename_(
#       "count"    =  "n"
#     ) %>%
#     dplyr::mutate(
#       proportion = count / sum(count)
#     )
#   d_summary$percentage <- base::paste0(base::round(d_summary$proportion*100), "%")
# 
#   y_title <- base::paste0(y_title, " (n=", scales::comma(base::sum(d_summary$count)), ")")
# 
#   g <-
#     ggplot(d_summary, aes_string(x="iv", y="count", fill="iv", label="percentage")) +
#     geom_bar(stat="identity") +
#     geom_text(stat="identity", size=text_size_percentage, hjust=.8, na.rm=T) +
#     scale_y_continuous(labels=scales::comma_format()) +
#     labs(title=main_title, x=x_title, y=y_title) +
#     coord_flip()
# 
#   theme  <-
#     theme_light(base_size=font_base_size) +
#     theme(legend.position       =  "none") +
#     theme(panel.grid.major.y    =  element_blank()) +
#     theme(panel.grid.minor.y    =  element_blank()) +
#     theme(axis.text.y           =  element_text(size=font_base_size + 2L)) +
#     theme(axis.text.x           =  element_text(colour="gray40")) +
#     theme(axis.title.x          =  element_text(colour="gray40")) +
#     theme(panel.border          =  element_rect(colour="gray80")) +
#     theme(axis.ticks            =  element_blank())
# 
#   return( g + theme )
# }
# histogram_continuous <- function(
#   d_observed,
#   variable_name,
#   bin_width               = NULL,
#   main_title              = base::gsub("_", " ", variable_name, perl=TRUE),
#   x_title                 = paste0(variable_name, "\n(each bin is ", scales::comma(bin_width), " units wide)"),
#   y_title                 = "Frequency",
#   rounded_digits          = 0L,
#   font_base_size          = 12
# ) {
# 
#   if( !inherits(d_observed, "data.frame") )
#     stop("`d_observed` should inherit from the data.frame class.")
# 
#   d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]), ]
# 
#   ds_mid_points <- base::data.frame(label=c("italic(X)[50]", "bar(italic(X))"), stringsAsFactors=FALSE)
#   ds_mid_points$value <- c(stats::median(d_observed[[variable_name]]), base::mean(d_observed[[variable_name]]))
#   ds_mid_points$value_rounded <- base::round(ds_mid_points$value, rounded_digits)
# 
#   if( ds_mid_points$value[1] < ds_mid_points$value[2] ) {
#     h_just <- c(1, 0)
#   } else {
#     h_just <- c(0, 1)
#   }
# 
#   g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x=variable_name))
#   g <- g + ggplot2::geom_histogram(binwidth=bin_width, position=ggplot2::position_identity(), fill="gray70", color="gray90", alpha=.7)
#   g <- g + ggplot2::geom_vline(xintercept=ds_mid_points$value, color="gray30")
#   g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y=0, label="value_rounded"), color="tomato", hjust=h_just, vjust=.5, na.rm=T)
#   g <- g + ggplot2::scale_x_continuous(labels=scales::comma_format())
#   g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
#   g <- g + ggplot2::labs(title=main_title, x=x_title, y=y_title)
# 
#   g <-
#     g + ggplot2::theme_light(base_size = font_base_size) +
#     ggplot2::theme(axis.ticks             = ggplot2::element_blank())
# 
#   ds_mid_points$top <- stats::quantile(ggplot2::ggplot_build(g)$layout$panel_ranges[[1]]$y.range, .8)
#   g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y="top", label="label"), color="tomato", hjust=h_just, parse=TRUE, na.rm=T)
#   return( g )
# }

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv(path_input) # 'ds' stands for 'datasets'

# ---- tweak-data --------------------------------------------------------------

ds <- ds0 %>%
  # ds_unique <- ds %>% 
  dplyr::filter(!is.na(response_value)) %>% 
  # dplyr::filter(component == "Common") %>%
  # dplyr::filter(component == "Unique") %>%
  # dplyr::filter(subject == "Contribution") %>%
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>%
  dplyr::mutate(
    qpretty = paste0("[",qid,"] - ",qlabel),
    common_item_id    = paste0(qid_fellow,"-", qid_host,"-",qid_academic),
    common_item_label = paste0("[",common_item_id,"] - ", qlabel),
    # so that no "NA" is displayed in the value:
    common_item_id    = gsub("NA","",common_item_id),
    common_item_label = gsub("NA","",common_item_label)
    # wrap the text of the display label
    # ,common_item_label_new = wrap_text(common_item_label, 80)
    # ,common_item_label_new = break_into_lines(common_item_label, line_length = 80)
  )
ds_common %>% dplyr::glimpse()
# unique(ds_common$common_item_label)[25]
# unique(ds_common$common_item_label_new)[25]
# text_string = "[--48] - Revealing the strengths and weaknesses of the current academic training environment, and opportunities for curriculum enhancement within HSPR"
# wrap_text(text_string, width_value = 80)


# ---- basic-questions ----------------------------
ds %>% dplyr::glimpse()
# How many people participated in the survey?
ds %>% 
  dplyr::group_by(respondent_role) %>% 
  dplyr::summarize( n = length(unique(response_id)))

d %>% dplyr::glimpse()
# what was response rate to the survey questions?
d <- ds %>% 
  dplyr::group_by(target) %>% 
  dplyr::mutate( 
    n_group = length(unique(response_id))
  ) %>% 
  dplyr::group_by(target, common_item_label) %>% 
  dplyr::mutate( 
    n_item = length(unique(response_id))
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    item_response_pct = scales::percent(n_item / n_group) 
  ) %>% 
  dplyr::select()

  tidyr::spread( key = target, value = n )
  



readr::write_csv(ds_descriptives, "./analysis/report-1/products/ds_descriptives.csv" )



