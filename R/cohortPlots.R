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
getPsPlot <- function(preference_score_dist,
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
  
  
  # #labelling see lines 455 to 481
  # unitOfAnalysis <- "subjects"
  # labelsLeft <- c()
  # #labelsRight <- c()
  # labelsLeft <- c(labelsLeft, sprintf("%s: %s %s", targetLabel, format(sum(as.numeric(d$treatment) == 1), big.mark = ",", scientific = FALSE), unitOfAnalysis))
  # labelsLeft <- c(labelsLeft, sprintf("%s: %s %s", comparatorLabel, format(sum(as.numeric(d$treatment) == 2), big.mark = ",", scientific = FALSE), unitOfAnalysis))
  # 
  # # auc <- CohortMethod::computePsAuc(data, confidenceIntervals = FALSE)
  # # labelsRight <- c(labelsRight, sprintf("AUC:\t\t%0.2f", auc))
  # 
  # # equipoise <- mean(data$preferenceScore >= 0.3 & data$preferenceScore <= 0.7)
  # # labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise", equipoise*100))
  # 
  # dummyLeft <- data.frame(text = paste(labelsLeft, collapse = "\n"))
  # yLine <- max(d$y) * 1.24
  # # dummyRight <- data.frame(text = paste(labelsRight, collapse = "\n"))
  # plot <- plot + 
  #   ggplot2::geom_label(
  #     x = 0, 
  #     y = 4, 
  #     hjust = "left", 
  #     vjust = "top",
  #     alpha = 0.8, 
  #     ggplot2::aes(label = text), 
  #     data = dummyLeft, 
  #     size = 3.5) 
  #   # ggplot2::geom_label(
  #   #   x = 1, 
  #   #   y =  max(d$y) * 1.24, 
  #   #   hjust = "right", 
  #   #   vjust = "top", 
  #   #   alpha = 0.8, 
  #   #   ggplot2::aes(label = text), 
  #   #   data = dummyRight, 
  #   #   size = 3.5)
  
  return(plot)
  
}
