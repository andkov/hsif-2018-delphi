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
ds <- readr::read_csv(path_input) # 'ds' stands for 'datasets'

# ---- tweak-data --------------------------------------------------------------


# ---- marginals ---------------------------------------------------------------



ds_common <- ds %>% 
  dplyr::filter(!is.na(response_value)) %>% 
  dplyr::filter(component == "Common") %>%
  # dplyr::filter(subject == "Contribution") %>% 
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>%
  dplyr::mutate(
    qpretty = paste0("[",qid,"] - ",qlabel),
    common_item_id    = paste0(qid_fellow,"-", qid_host,"-",qid_academic),
    common_item_label = paste0("[",common_item_id,"] - ", qlabel)#,
    
    # common_item_id    = ifelse(component == "Unique", qid, common_item_id),
    # common_item_label = ifelse(component == "Unique", qpretty, common_item_label)
  )



library(ggplot2)

# define the graphing function to distribution in the matrix plot
histogram_continuous <- function (
  d_observed, 
  variable_name, 
  bin_width      = NULL, 
  main_title     = base::gsub("_", " ", variable_name, perl = TRUE), 
  sub_title      = NULL, 
  caption        = paste0("each bin is ", scales::comma(bin_width), " units wide"), 
  x_title        = variable_name, 
  y_title        = "Frequency", 
  x_axis_format  = scales::comma_format(), 
  rounded_digits = 0L, 
  font_base_size = 12
) 
{
  # browser()
  # d_observed     = ds_common %>% dplyr::filter(common_item_id == "3-22-42") %>% dplyr::filter(target == "Host")
  # # d_observed     = ds_common %>% dplyr::filter(common_item_id == "9") #%>% dplyr::filter(target == "Host")
  # variable_name  = "response_value"
  # bin_width      = 1
  # main_title     = base::gsub("_", " ", variable_name, perl = TRUE)
  # sub_title      = NULL
  # caption        = paste0("each bin is ", scales::comma(bin_width), " units wide")
  # x_title        = variable_name
  # y_title        = "Frequency"
  # x_axis_format  = scales::comma_format()
  # rounded_digits = 2L
  # font_base_size = 12

  
  
  
  if (!inherits(d_observed, "data.frame")) 
    stop("`d_observed` should inherit from the data.frame class.")
  d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]), 
                           ]
  non_empty <- (nrow(d_observed) >= 1L)
  if (non_empty) {
    ds_mid_points <- base::data.frame(label = c("bar(italic(X))","italic(SD)"), stringsAsFactors = FALSE )
    ds_mid_points$value <- c(base::mean(d_observed[[variable_name]]),
                             stats::sd(d_observed[[variable_name]]))
    ds_mid_points$value_rounded <- sprintf("%.*f", rounded_digits, ds_mid_points$value)
    ds_mid_points$line_position <- ds_mid_points$value[1]
    # if (ds_mid_points$value[1] < ds_mid_points$value[2]) {
      h_just <- c(1.5, -0.2)
      # h_just <- c(1.4)
      # h_just <- c(-0.2)
    # }
    # else {
    #   h_just <- c(-0.1, 1.1)
    # }
  } else {
    main_title <- paste0("Empty: ", main_title)
    caption <- "The variable contains only missing values.\nThere is nothing to graph."
    ds_mid_points <- tibble::tribble(~label, ~value, ~value_rounded, 
                                     "italic(X)[50]", NA_real_, NA_character_, "bar(italic(X))", 
                                     NA_real_, NA_character_)
    h_just <- c(1.2, -0.2)
    # h_just <- c(1.1)
    # h_just <- c(0.2)
  }
  palette_midpoint <- c("#2274A5", "#32936F")
  palette_midpoint_line <- c("#2274A5", "#2274A5")
  # palette_midpoint <- c("#2274A5")
  # rename input for faceting
  g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x = variable_name)) + 
    ggplot2::geom_histogram(binwidth = bin_width ,position = ggplot2::position_identity()
      ,fill = "gray92", color = "gray80", size = 1, alpha = 0.7) + 
    ggplot2::geom_vline(xintercept = ds_mid_points$line_position,color = palette_midpoint_line, na.rm = T) +
    # ggplot2::geom_vline(xintercept = rep(ds_mid_points$value[1],2),color = palette_midpoint_line, na.rm = T) +
    ggplot2::geom_text( data    = ds_mid_points,
                        mapping =  ggplot2::aes_string(x = "line_position", y = -Inf, label = "value_rounded"),
                        color   = palette_midpoint,
                        hjust   = h_just,
                        vjust   = -1.2,
                        na.rm   = T
                        ) +
    ggplot2::geom_text(data    = ds_mid_points,
                       mapping = ggplot2::aes_string(x = "line_position", y = Inf, label = "label"),
                       color   = palette_midpoint,
                       hjust = h_just,
                       vjust = 1.2,
                       parse   = TRUE,
                       na.rm = T
                       ) +
    ggplot2::scale_x_continuous(labels = x_axis_format, breaks = 1:5, limits = c(.5,5.5)) + 
    # ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(-.5, 24)) + 
    ggplot2::scale_y_continuous(labels = scales::comma_format() ) + 
    ggplot2::coord_cartesian(ylim = c(0,20))
    ggplot2::labs(
      title = main_title, subtitle = sub_title, 
      caption = caption, x = x_title, y = y_title
    )
    
  
  g <- g + ggplot2::theme_light(base_size = font_base_size) + 
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) + 
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "gray90")) + 
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "gray94")) + 
    ggplot2::theme(plot.caption = ggplot2::element_text(color = "gray60")) + 
    ggplot2::theme(axis.title.y = ggplot2::element_text(color = "gray60"))
  # g
  return(g)
}
 
# g1 <- ds_common %>% 
#   dplyr::filter(subject == "Contribution") %>% 
#   dplyr::filter(common_item_id == "3-22-42") %>% 
#   dplyr::filter(target == "Host") %>% 
#   histogram_continuous("response_value",bin_width = 1, rounded_digits = 2)
# g1

# define complext plot, matrix of simple views
matrix_plot <- function(
  d, # ds_long
  # patterns
  # ,
  facet_x = "target",
  facet_y = "common_item_id"
){
  # values for testing
  # d <- ds_common %>% dplyr::filter(subject == "Contribution")
  # facet_x = "target"
  # facet_y = "common_item_label"
  # facet_y = "common_item_id"

  # create a list of plots to facet with ggmatrix
  # (facet_y_values <- unique(d$common_item_id))# %>% list() %>% unlist()
  facet_y_values <- unique(d$common_item_label)# %>% list() %>% unlist()
  (facet_x_values <- unique(d$target)) #%>% list() %>% unlist()#"Fellow"   "Host"     "Academic"
  ls <- list()
  for(i_item in (facet_y_values)){
    for(i_role in (facet_x_values)){
      
    ls[[paste0(i_item,"-",i_role)]] <- d %>% 
      # dplyr::filter(common_item_id == i_item) %>%
      dplyr::filter(common_item_label == i_item) %>%
      dplyr::filter(target == i_role) %>% 
      histogram_continuous(variable_name = "response_value", bin_width = 1,
                           rounded_digits = 2,
                           main_title = paste0(i_item, " - ", i_role))
    } 
  } 
  # ls$`[1-20-40] - Research/analytic skills and expertise-Host`
  # ls$`[1-20-40] - Research/analytic skills and expertise-Fellow`
  # place the plots into a single ggmatrix
  mplot <- GGally::ggmatrix(
    ls
    ,ncol = length(facet_x_values)
    ,nrow = length(facet_y_values)
    # title = "Observed MMSE scores for three types of response patterns",
    # yAxisLabels = patterns,
    ,yAxisLabels = facet_y_values
    ,xAxisLabels = facet_x_values
    # xlab = "Years since baseline", ylab = "Mini Mental State Exam (MMSE) Score"
 
    # legend = 1
  ) + theme(
    legend.position = "right"
    ,strip.text.x = element_text(size=baseSize+2)
    ,strip.text.y = element_text(size=baseSize+2, angle = 0, hjust = 0)
    
  )
  mplot
}
# usage demo:
# ds_common %>% dplyr::filter(subject == "Contribution") %>% matrix_plot()
ds_common %>% dplyr::filter(subject == "Criterion") %>% matrix_plot()


ds_descriptives <- ds_common %>% 
  # dplyr::filter(qid == 1) %>% 
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>% 
  dplyr::group_by(qid, qlabel, respondent_role,target, subject, component) %>%
  # dplyr::group_by(qid, qlabel) %>% 
  dplyr::summarize(
    n_responses = n(),
    mean = mean(response_value, na.rm = T),
    sd   = sd(response_value,na.rm = T)
  )






g1 <- ds_common %>% 
  # histogram_continuous("response_value", bin_width = 1 ) #+
  # histogram_continuous("response_value", bin_width = 1 ,rounded_digits = 2) #+
  facet_grid(. ~ target)+
  TabularManifest::histogram_continuous("response_value", bin_width = 1 ,rounded_digits = 2) 
# facet_grid(common_contribution_item ~ target)
# theme()
g1
ggplot2::ggplot(aes_string( x = "common_contribution_item" ,y = "response_value" ) )+
  # ggplot2::ggplot(aes_string( y = "qpretty" ,x = "response_value" ) )+
  # geom_boxplot(position = "dodge")+
  # geom_boxplot()+
  geom_jitter(shape = 21, fill = NA, alpha = .5)+
  facet_grid(.~respondent_role)+
  coord_flip()+
  
  theme_bw()
g1

# Uncomment the next line for a dynamic, JavaScript [DataTables](https://datatables.net/) table.
# DT::datatable(round(summary(m2)$coef, digits = 2), options = list(pageLength = 2))
