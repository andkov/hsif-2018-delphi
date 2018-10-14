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
    if (!inherits(d_observed, "data.frame")) 
        stop("`d_observed` should inherit from the data.frame class.")
    d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]), 
        ]
    non_empty <- (nrow(d_observed) >= 1L)
    if (non_empty) {
        ds_mid_points <- base::data.frame(label = c("italic(X)[50]", 
            "bar(italic(X))"), stringsAsFactors = FALSE)
        ds_mid_points$value <- c(stats::median(d_observed[[variable_name]]), 
            base::mean(d_observed[[variable_name]]))
        ds_mid_points$value_rounded <- sprintf("%.*f", rounded_digits, 
            ds_mid_points$value)
        if (ds_mid_points$value[1] < ds_mid_points$value[2]) {
            h_just <- c(1.1, -0.1)
        }
        else {
            h_just <- c(-0.1, 1.1)
        }
    }
    else {
        main_title <- paste0("Empty: ", main_title)
        caption <- "The variable contains only missing values.\nThere is nothing to graph."
        ds_mid_points <- tibble::tribble(~label, ~value, ~value_rounded, 
            "italic(X)[50]", NA_real_, NA_character_, "bar(italic(X))", 
            NA_real_, NA_character_)
        h_just <- c(1.1, -0.1)
    }
    palette_midpoint <- c("#2274A5", "#32936F")
    g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x = variable_name)) + 
        ggplot2::geom_histogram(binwidth = bin_width, position = ggplot2::position_identity(), 
            fill = "gray92", color = "gray80", size = 1, alpha = 0.7) + 
        ggplot2::geom_vline(xintercept = ds_mid_points$value, 
            color = palette_midpoint, na.rm = T) + ggplot2::geom_text(data = ds_mid_points, 
        ggplot2::aes_string(x = "value", y = -Inf, label = "value_rounded"), 
        color = palette_midpoint, hjust = h_just, vjust = -0.2, 
        na.rm = T) + ggplot2::geom_text(data = ds_mid_points, 
        ggplot2::aes_string(x = "value", y = Inf, label = "label"), 
        color = palette_midpoint, hjust = h_just, vjust = 1.2, 
        parse = TRUE, na.rm = T) + ggplot2::scale_x_continuous(labels = x_axis_format) + 
        ggplot2::scale_y_continuous(labels = scales::comma_format()) + 
        ggplot2::labs(title = main_title, subtitle = sub_title, 
            caption = caption, x = x_title, y = y_title)
    g <- g + ggplot2::theme_light(base_size = font_base_size) + 
        ggplot2::theme(axis.ticks = ggplot2::element_blank()) + 
        ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "gray90")) + 
        ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "gray94")) + 
        ggplot2::theme(plot.caption = ggplot2::element_text(color = "gray60")) + 
        ggplot2::theme(axis.title.y = ggplot2::element_text(color = "gray60"))
    return(g)
}