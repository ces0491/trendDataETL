% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_drive.R
\name{update_processed_file}
\alias{update_processed_file}
\title{Update the processed_files table after successful processing}
\usage{
update_processed_file(conn, file_id, file_name, platform_id, modified_time)
}
\arguments{
\item{conn}{Database connection}

\item{file_id}{Google Drive file ID}

\item{file_name}{File name}

\item{platform_id}{Platform ID}

\item{modified_time}{File modification time}
}
\value{
TRUE if successful
}
\description{
Update the processed_files table after successful processing
}
