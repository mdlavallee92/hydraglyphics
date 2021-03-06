% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohortPlots.R
\name{getPreferenceDensity}
\alias{getPreferenceDensity}
\title{Plot Preference score distribution}
\usage{
getPreferenceDensity(
  preference_score_dist,
  filePath = TRUE,
  target_id,
  comparator_id,
  analysis_id
)
}
\arguments{
\item{preference_score_dist}{either a valid csv path, dataframe or tibble. If using a csv path
filePath must be true}

\item{filePath}{a logic toggle indicating whether a file path is used for preference_score_dist}

\item{target_id}{the target id to extract from the aggregate file}

\item{comparator_id}{the comparator id to extract from the aggregate file}

\item{analysis_id}{the analysis id to exctract from the aggregate file}
}
\value{
a ggplot object of the preference distribribution
}
\description{
This function takes the preference score distribution csv file from a Hydra
package and creates a density plot
}
