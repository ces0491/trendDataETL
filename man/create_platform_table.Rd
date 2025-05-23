% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{create_platform_table}
\alias{create_platform_table}
\title{Create platform data table for a specific platform}
\usage{
create_platform_table(conn, platform_id, db_type = "sqlite")
}
\arguments{
\item{conn}{Database connection}

\item{platform_id}{Platform ID}

\item{db_type}{Database type}
}
\value{
TRUE if successful
}
\description{
Create platform data table for a specific platform
}
