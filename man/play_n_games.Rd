% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_n_games}
\alias{play_n_games}
\title{#'   Play the Monty Hall game as many times as you'd like!}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{...}{the n argument takes an integer.}
}
\value{
The function returns a data frame of the outcomes.
}
\description{
#'   \code{play_n_games()} is much like the previous function where
you can run the entire setup, but this function allows for
an argument of how many games to be played.
}
\details{
#'   The Monty Hall game allows for some serious consideration
of probability statistics. Knowing that some will want to
run this game several times in order to see the chances
and study the options, this function has a built in number
taker that determines how many times the game will be played.
}
\examples{
  play_n_games(100)
  
}
