% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_stats.R
\name{count_the_speakers}
\alias{count_the_speakers}
\title{Count who speakes most by number of times}
\usage{
count_the_speakers(all_scripts, type, season = "all", episode = "all",
  scaled = FALSE)
}
\arguments{
\item{all_scripts}{a dataframe with all the scripts data arranged}

\item{type}{a character indicates if to check number of times a charcter
speaks ("num_speaks") or number of words ("num_words")}

\item{season}{a number indicates the season. for all season can omit or "all"}

\item{episode}{a number indicates the episode. for all season can omit or "all"}

\item{scaled}{logical. Weather to scale the frequency by number of episodes the character involved.}
}
\value{
a frequency table
}
\description{
A function that counts who speaks the most number of times
}

