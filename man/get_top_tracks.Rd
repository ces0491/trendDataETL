% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation.R
\name{get_top_tracks}
\alias{get_top_tracks}
\title{Get top tracks for a specific platform and time period}
\usage{
get_top_tracks(
  conn,
  platform_id,
  start_date,
  end_date,
  country = NULL,
  limit = 50
)
}
\arguments{
\item{conn}{Database connection}

\item{platform_id}{Platform ID}

\item{start_date}{Start date (YYYY-MM-DD)}

\item{end_date}{End date (YYYY-MM-DD)}

\item{country}{Optional country filter}

\item{limit}{Number of top tracks to return (default: 50)}
}
\value{
Data frame with top tracks
}
\description{
Get top tracks for a specific platform and time period
}
