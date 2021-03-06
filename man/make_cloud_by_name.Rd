% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/word_clouds.R
\name{make_cloud_by_name}
\alias{make_cloud_by_name}
\title{Make a word cloud from a dataframe}
\usage{
make_cloud_by_name(all_scripts, name, season = "all", episode = "all",
  stopwords = FALSE, stemming = FALSE)
}
\arguments{
\item{all_scripts}{a dataframe to make cloud from}

\item{stopwords}{logical. Indicates weather to remove stopwords or not.}

\item{stemming}{logical. Indicates weather to use stemming algorithm.}
}
\value{
A word cloud chart
}
\description{
Make a word cloud from a dataframe
}

