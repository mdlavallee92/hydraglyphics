#' Plot Preference score distribution
#' 
#' This function takes the preference score distribution csv file from a Hydra 
#' package and creates a density plot
#' @param preference_score_dist either a valid csv path, dataframe or tibble. If using a csv path
#' filePath must be true
#' @param filePath a logic toggle indicating whether a file path is used for preference_score_dist
#' @param target_id the target id to extract from the aggregate file
#' @param comparator_id the comparator id to extract from the aggregate file
#' @param analysis_id the analysis id to exctract from the aggregate file
#' @import magrittr
#' @return a ggplot object of the preference distribribution
#' @export
getPreferenceDensity <- function(preference_score_dist,
                      filePath = TRUE,
                      target_id,
                      comparator_id,
                      analysis_id) {
  
  #check if preference score dist is a path string and if it exists
  if (is.character(preference_score_dist) & filePath) {
    if (file.exists(preference_score_dist)) {
      #read in 
      preference_score_dist <- readr::read_csv(preference_score_dist,
                                               col_types = readr::cols())
    } else{
      stop("preference_score_dist must be a data frame, tibble or a valid path to a csv file")
    }
  } else {
    stop("preference_score_dist must be a data frame, tibble or a valid path to a csv file")
  }
  
  #check if preference score dist is a dataframe
  if (!tibble::is_tibble(preference_score_dist) | !is.data.frame(preference_score_dist)) {
    stop("preference_score_dist must be a data frame, tibble or a valid path to a csv file")
  }
  
  
  data <- preference_score_dist %>%
    dplyr::filter(target_id == !!target_id, comparator_id == !!comparator_id, analysis_id == !!analysis_id)
  
  targetLabel <- "Target"
  comparatorLabel <- "Comparator"
  label <- "Preference score"
  #Adopted from CohortMethod::plotPs (See L346:489 of PsFunctions.R)
  #create plotting data frame
  d <- data.frame(
    x = c(data$preference_score, data$preference_score),
    y = c(data$target_density, data$comparator_density), 
    treatment = c(rep(as.character(targetLabel), length(data$preference_score)),
                  rep(as.character(comparatorLabel), length(data$preference_score)))
  )
  d$treatment <- factor(d$treatment, levels = c(targetLabel, comparatorLabel))
  
  #create density plot
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_density(stat = "identity", ggplot2::aes(color = .data$treatment, group = .data$treatment, fill = .data$treatment)) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density", limits = c(0, max(d$y))) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))) +
    ggplot2::labs(
      title = "Preference Score Distribution",
      subtitle = paste(paste("Target ID:", target_id), 
                       paste("Comparator ID:", comparator_id), 
                       paste("Analysis ID:", analysis_id), sep = "; ")
    )
  
  
  return(plot)
  
}


#' Plot Covariate Balance
#' 
#' This function takes the covariate balance and shows the balance scatter plot 
#' @param balance either a valid csv path, dataframe or tibble. If using a csv path
#' filePath must be true
#' @param filePath a logic toggle indicating whether a file path is used for balance
#' @param target_id the target id to extract from the aggregate file
#' @param comparator_id the comparator id to extract from the aggregate file
#' @param outcome_id the outcome id to extract from the aggregate file
#' @param analysis_id the analysis id to exctract from the aggregate file
#' @import magrittr
#' @return a ggplot object of the covariate balance scatter plot
#' @export
getBalanceScatter <- function(balance,
                                 filePath = TRUE,
                                 target_id,
                                 comparator_id,
                                 outcome_id,
                                 analysis_id) {
  
  #check if preference score dist is a path string and if it exists
  if (is.character(balance) & filePath) {
    if (file.exists(balance)) {
      #read in 
      balance <- readr::read_csv(balance,
                                 col_types = readr::cols())
    } else{
      stop("balance must be a data frame, tibble or a valid path to a csv file")
    }
  } else {
    stop("balance must be a data frame, tibble or a valid path to a csv file")
  }
  
  #check if preference score dist is a dataframe
  if (!tibble::is_tibble(balance) | !is.data.frame(balance)) {
    stop("balance must be a data frame, tibble or a valid path to a csv file")
  }
  
  data <- balance %>%
    dplyr::filter(target_id == !!target_id, 
                  comparator_id == !!comparator_id, 
                  outcome_id == !!outcome_id, 
                  analysis_id == !!analysis_id)
  
  #scatter plot
  beforeLabel <- as.character("Before matching")
  afterLabel <- as.character("After matching")
  data$beforeMatchingStdDiff <- abs(data$std_diff_before)
  data$afterMatchingStdDiff <- abs(data$std_diff_after)
  
  
  limits <- c(min(c(data$beforeMatchingStdDiff, data$afterMatchingStdDiff), na.rm = TRUE),
              max(c(data$beforeMatchingStdDiff, data$afterMatchingStdDiff), na.rm = TRUE))
  plot <- ggplot2::ggplot(data,
                          ggplot2::aes(x = .data$beforeMatchingStdDiff, y = .data$afterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle("Standardized difference of mean") +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits)
  
  
  labels <- c()
  labels <- c(labels, sprintf("Number of covariates: %s", format(nrow(data), big.mark = ",", scientific = FALSE)))
  labels <- c(labels, sprintf("%s max(absolute): %.2f", afterLabel, max(abs(data$afterMatchingStdDiff), na.rm = TRUE)))
  dummy <- data.frame(text = paste(labels, collapse = "\n"))
  plot <- plot + ggplot2::geom_label(x = limits[1] + 0.01, y = limits[2], hjust = "left", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)
  
  return(plot)
  
}
