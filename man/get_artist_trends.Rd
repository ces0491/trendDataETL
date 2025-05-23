% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation.R
\name{get_artist_trends}
\alias{get_artist_trends}
\title{Get trending data for an artist across all platforms}
\usage{
get_artist_trends(conn, artist_name, start_date = NULL, end_date = NULL)
}
\arguments{
\item{conn}{Database connection}

\item{artist_name}{Artist name}

\item{start_date}{Start date (YYYY-MM-DD)}

\item{end_date}{End date (YYYY-MM-DD)}
}
\value{
Data frame with trending data
}
\description{
Get trending data for an artist across all platforms
}
