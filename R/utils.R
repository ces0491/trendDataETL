# utils.R
# Utility functions and main package wrapper functions

#' Set up logging
#'
#' @param log_level Logging level (default: "INFO")
#' @param log_file Optional log file path
#' @return TRUE if successful
#' @export
setup_logging <- function(log_level = "INFO", log_file = NULL) {
  tryCatch({
    if (!requireNamespace("logger", quietly = TRUE)) {
      stop("Package 'logger' is required. Please install it.")
    }

    # Set log level
    logger::log_threshold(log_level)

    # Set log file if provided
    if (!is.null(log_file)) {
      # Create the directory if it doesn't exist
      log_dir <- dirname(log_file)
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }

      logger::log_appender(logger::appender_file(log_file))
    }

    logger::log_info("Logging initialized with level: ", log_level)
    return(TRUE)
  }, error = function(e) {
    message("Error setting up logging: ", e$message)
    return(FALSE)
  })
}

#' Initialize the MusicStreamTrends system
#'
#' @param db_type Database type (default: "sqlite")
#' @param db_path Database path (default: "music_streaming.db")
#' @param db_user Database username (not needed for SQLite)
#' @param db_password Database password (not needed for SQLite)
#' @param db_host Database host (not needed for SQLite)
#' @param db_port Database port (not needed for SQLite)
#' @param log_level Logging level (default: "INFO")
#' @param log_file Optional log file path
#' @param auth_token Optional Google Drive auth token path
#' @return List with connection and platform information
#' @export
initialize_system <- function(db_type = "sqlite", db_path = "music_streaming.db",
                              db_user = NULL, db_password = NULL,
                              db_host = NULL, db_port = NULL,
                              log_level = "INFO", log_file = NULL,
                              auth_token = NULL) {
  # Set up logging
  setup_logging(log_level, log_file)

  # Set up database connection
  conn <- setup_database(db_type, db_path, db_user, db_password, db_host, db_port)

  # Initialize platforms
  platforms <- define_platforms()
  initialize_platforms(conn)

  # Authenticate with Google Drive if needed
  if (!is.null(auth_token)) {
    setup_gdrive_auth(auth_token)
  }

  # Return system objects
  return(list(
    conn = conn,
    platforms = platforms,
    db_type = db_type,
    initialized = TRUE
  ))
}

#' Process all DSP data from Google Drive
#'
#' @param system System object from initialize_system
#' @param parent_folder_id Google Drive parent folder ID
#' @param date_pattern Regex pattern to identify date folders (default: "^[0-9]{6}$")
#' @param date_range Optional vector of length 2 with start and end dates
#' @param temp_dir Temporary directory for downloads (default: tempdir())
#' @return Total number of files processed
#' @export
process_all_dsp_data <- function(system, parent_folder_id,
                                 date_pattern = "^[0-9]{6}$",
                                 date_range = NULL,
                                 temp_dir = tempdir()) {

  tryCatch({
    # Check if system is initialized
    if (!exists("initialized", system) || !system$initialized) {
      stop("System not initialized. Call initialize_system() first.")
    }

    # Check Google Drive authentication
    if (!requireNamespace("googledrive", quietly = TRUE)) {
      stop("Package 'googledrive' is required. Please install it.")
    }

    if (!googledrive::drive_has_token()) {
      logger::log_info("Google Drive authentication required")
      setup_gdrive_auth()
    }

    # Process all folders with nested structure
    total_processed <- process_nested_folders(
      system$conn,
      parent_folder_id,
      date_pattern,
      date_range,
      system$platforms,
      temp_dir
    )

    # Create aggregated views
    create_aggregated_views(system$conn)

    # Clean up temporary files
    cleanup_temp_files(temp_dir)

    logger::log_info("Processed ", total_processed, " files in total")
    return(total_processed)
  }, error = function(e) {
    logger::log_error("Error processing DSP data: ", e$message)
    return(0)
  })
}

#' Generate a comprehensive report of all platform data
#'
#' @param system System object from initialize_system
#' @param output_dir Output directory for the report
#' @return Path to the report file
#' @export
generate_platform_report <- function(system, output_dir = ".") {
  tryCatch({
    # Check if system is initialized
    if (!exists("initialized", system) || !system$initialized) {
      stop("System not initialized. Call initialize_system() first.")
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Generate report file name
    report_file <- file.path(output_dir, paste0("platform_report_", format(Sys.Date(), "%Y%m%d"), ".html"))

    # Get platform summary
    platform_summary <- get_platform_summary(system$conn)

    if (nrow(platform_summary) == 0) {
      logger::log_warn("No platform data available for report")
      return(NULL)
    }

    # Generate HTML report
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      # Create temporary Rmd file
      rmd_template <- system.file("templates", "platform_report.Rmd", package = "MusicStreamTrends")

      if (rmd_template == "") {
        # Template not found, create one
        rmd_template <- file.path(tempdir(), "platform_report.Rmd")

        rmd_content <- '---
title: "Music Streaming Platforms Data Report"
date: "`r format(Sys.Date(), "%B %d, %Y")`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(DT)
```

## Platform Summary

```{r}
DT::datatable(platform_summary, options = list(pageLength = 15))
```

## Data Availability

```{r}
ggplot(platform_summary, aes(x = platform_name, y = days_with_data, fill = platform_name)) +
  geom_col() +
  labs(title = "Days with Data by Platform", x = "Platform", y = "Number of Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Date Ranges

```{r}
platform_summary$first_date <- as.Date(platform_summary$first_date)
platform_summary$last_date <- as.Date(platform_summary$last_date)

date_df <- data.frame(
  platform = rep(platform_summary$platform_name, each = 2),
  date_type = rep(c("First Date", "Last Date"), nrow(platform_summary)),
  date = c(rbind(platform_summary$first_date, platform_summary$last_date))
)

ggplot(date_df, aes(x = platform, y = date, color = date_type)) +
  geom_point(size = 3) +
  labs(title = "Data Date Ranges by Platform", x = "Platform", y = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Record Counts

```{r}
ggplot(platform_summary, aes(x = platform_name, y = total_records, fill = platform_name)) +
  geom_col() +
  labs(title = "Total Records by Platform", x = "Platform", y = "Number of Records") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()
```'

  writeLines(rmd_content, rmd_template)
      }

      # Render the report
      rmarkdown::render(
        rmd_template,
        output_file = report_file,
        params = list(
          platform_summary = platform_summary,
          conn = system$conn
        ),
        envir = new.env()
      )

      logger::log_info("Generated platform report: ", report_file)
      return(report_file)
    } else {
      # Fall back to CSV if rmarkdown not available
      csv_file <- file.path(output_dir, paste0("platform_summary_", format(Sys.Date(), "%Y%m%d"), ".csv"))
      utils::write.csv(platform_summary, csv_file, row.names = FALSE)

      logger::log_warn("Package 'rmarkdown' not available. Generated CSV summary: ", csv_file)
      return(csv_file)
    }
  }, error = function(e) {
    logger::log_error("Error generating platform report: ", e$message)
    return(NULL)
  })
}

#' Run a complete DSP data update
#'
#' @param parent_folder_id Google Drive parent folder ID
#' @param db_path Database path (default: "music_streaming.db")
#' @param auth_token Optional Google Drive auth token path
#' @param date_range Optional vector of length 2 with start and end dates
#' @param generate_report Whether to generate a report (default: TRUE)
#' @param output_dir Output directory for reports and exports (default: "output")
#' @return List with processing results
#' @export
run_dsp_update <- function(parent_folder_id, db_path = "music_streaming.db",
                           auth_token = NULL, date_range = NULL,
                           generate_report = TRUE, output_dir = "output") {

  tryCatch({
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Initialize system
    system <- initialize_system(
      db_type = "sqlite",
      db_path = db_path,
      log_level = "INFO",
      log_file = file.path(output_dir, "dsp_update.log"),
      auth_token = auth_token
    )

    # Process all DSP data
    files_processed <- process_all_dsp_data(
      system,
      parent_folder_id,
      date_range = date_range
    )

    results <- list(
      files_processed = files_processed,
      report_file = NULL,
      summary = NULL
    )

    # Generate platform summary
    results$summary <- get_platform_summary(system$conn)

    # Generate report if requested
    if (generate_report) {
      results$report_file <- generate_platform_report(system, output_dir)
    }

    # Close database connection
    DBI::dbDisconnect(system$conn)

    logger::log_info("DSP update completed successfully")
    return(results)
  }, error = function(e) {
    logger::log_error("Error in DSP update: ", e$message)
    return(list(
      error = e$message,
      files_processed = 0
    ))
  })
}
