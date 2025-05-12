# file_processing.R
# Functions for file processing, decompression, and standardization

#' Process a file based on its type and extract if necessary
#'
#' @param file_path Path to the file
#' @param platform Platform configuration
#' @param temp_dir Temporary directory for extraction
#' @return Path to the processed file or vector of paths if multiple files extracted
#' @export
preprocess_file <- function(file_path, platform, temp_dir = tempdir()) {
  tryCatch({
    file_ext <- tolower(tools::file_ext(file_path))
    file_name <- basename(file_path)

    # Create a specific temp dir for this file
    file_temp_dir <- file.path(temp_dir, gsub("[^a-zA-Z0-9]", "_", file_name))
    if (!dir.exists(file_temp_dir)) {
      dir.create(file_temp_dir, recursive = TRUE)
    }

    if (file_ext == "zip") {
      logger::log_info("Extracting ZIP file: ", file_name)

      # List contents before extraction to check for nested archives
      zip_contents <- utils::unzip(file_path, list = TRUE)
      logger::log_debug("ZIP contains ", nrow(zip_contents), " files")

      # Extract all files
      utils::unzip(file_path, exdir = file_temp_dir)

      # Get all extracted files
      extracted_files <- list.files(file_temp_dir, full.names = TRUE, recursive = TRUE)

      # Process nested archives
      for (nested_file in extracted_files) {
        nested_ext <- tolower(tools::file_ext(nested_file))
        if (nested_ext %in% c("zip", "gz", "rar", "7z")) {
          # Recursively process nested archives
          nested_results <- preprocess_file(nested_file, platform, file_temp_dir)

          # Add nested results to extracted files
          extracted_files <- c(extracted_files, nested_results)

          # Remove the nested archive from the list
          extracted_files <- extracted_files[extracted_files != nested_file]
        }
      }

      logger::log_info("Extracted ", length(extracted_files), " files from ", file_name)
      return(extracted_files)

    } else if (file_ext == "gz" || file_ext == "gzip") {
      # For gzipped files
      logger::log_info("Extracting GZIP file: ", file_name)

      # Determine output filename by removing .gz extension
      output_file <- file.path(file_temp_dir, tools::file_path_sans_ext(basename(file_path)))

      # Extract using R's gzfile function
      con_in <- gzfile(file_path, "rb")
      con_out <- file(output_file, "wb")

      while (length(data <- readBin(con_in, raw(), 4096)) > 0) {
        writeBin(data, con_out)
      }

      close(con_in)
      close(con_out)

      logger::log_info("Extracted GZIP to: ", output_file)
      return(output_file)

    } else if (file_ext %in% c("csv", "txt", "tsv", "xls", "xlsx", "json")) {
      # No preprocessing needed for these file types
      logger::log_info("No extraction needed for file: ", file_name)
      return(file_path)

    } else {
      # Unknown file type - return original path
      logger::log_warn("Unknown file type for processing: ", file_ext)
      return(file_path)
    }
  }, error = function(e) {
    logger::log_error("Error preprocessing file: ", e$message)
    return(file_path)  # Return original path on error
  })
}

#' Read a data file based on its type
#'
#' @param file_path Path to the file
#' @param platform Platform configuration
#' @return Data frame containing the file contents
#' @export
read_data_file <- function(file_path, platform) {
  tryCatch({
    file_ext <- tolower(tools::file_ext(file_path))
    file_name <- basename(file_path)

    # Initialize empty dataframe
    df <- data.frame()

    if (file_ext == "csv") {
      # First try to read with readr for better performance and encoding handling
      if (requireNamespace("readr", quietly = TRUE)) {
        df <- readr::read_csv(file_path, guess_max = 10000, show_col_types = FALSE)
        df <- as.data.frame(df)
      } else {
        # Fall back to base R
        df <- utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      }

    } else if (file_ext == "tsv" || (file_ext == "txt" && grepl("\t", readLines(file_path, n = 1)))) {
      # Tab-separated files
      if (requireNamespace("readr", quietly = TRUE)) {
        df <- readr::read_tsv(file_path, guess_max = 10000, show_col_types = FALSE)
        df <- as.data.frame(df)
      } else {
        df <- utils::read.delim(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      }

    } else if (file_ext == "txt") {
      # Try to determine the separator
      first_line <- readLines(file_path, n = 1)

      if (grepl(",", first_line)) {
        sep <- ","
      } else if (grepl("\t", first_line)) {
        sep <- "\t"
      } else if (grepl(";", first_line)) {
        sep <- ";"
      } else if (grepl("\\|", first_line)) {
        sep <- "|"
      } else {
        # Default to comma
        sep <- ","
      }

      df <- utils::read.delim(file_path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)

    } else if (file_ext %in% c("xls", "xlsx")) {
      # Excel files
      if (requireNamespace("readxl", quietly = TRUE)) {
        df <- readxl::read_excel(file_path)
        df <- as.data.frame(df)
      } else {
        stop("Package 'readxl' is required to read Excel files. Please install it.")
      }

    } else if (file_ext == "json") {
      # JSON files
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        json_data <- jsonlite::fromJSON(file_path)

        # Handle different JSON structures
        if (is.data.frame(json_data)) {
          df <- json_data
        } else if (is.list(json_data) && length(json_data) > 0) {
          # Try to find a data frame in the list
          data_elements <- sapply(json_data, is.data.frame)

          if (any(data_elements)) {
            # Use the first data frame found
            df <- json_data[[which(data_elements)[1]]]
          } else {
            # Try to convert the list to a data frame
            df <- as.data.frame(json_data, stringsAsFactors = FALSE)
          }
        }
      } else {
        stop("Package 'jsonlite' is required to read JSON files. Please install it.")
      }

    } else {
      # Unsupported file type
      stop("Unsupported file type: ", file_ext)
    }

    logger::log_info("Read data file successfully: ", file_name, ", rows: ", nrow(df), ", cols: ", ncol(df))
    return(df)

  }, error = function(e) {
    logger::log_error("Error reading data file: ", e$message)
    return(data.frame())
  })
}

#' Standardize column names based on platform field mapping
#'
#' @param df Data frame to standardize
#' @param platform Platform configuration
#' @return Data frame with standardized column names
#' @export
standardize_columns <- function(df, platform) {
  tryCatch({
    original_cols <- names(df)
    new_cols <- original_cols
    field_map <- platform$field_map
    standardized <- FALSE

    for (i in seq_along(original_cols)) {
      # Check for exact matches in the field map
      if (original_cols[i] %in% names(field_map)) {
        new_cols[i] <- field_map[[original_cols[i]]]
        standardized <- TRUE
      } else {
        # Try case-insensitive matching
        match_idx <- which(tolower(original_cols[i]) == tolower(names(field_map)))
        if (length(match_idx) > 0) {
          new_cols[i] <- field_map[[names(field_map)[match_idx[1]]]]
          standardized <- TRUE
        }
      }
    }

    # Only rename if at least one column was standardized
    if (standardized) {
      names(df) <- new_cols
      logger::log_info("Standardized columns for platform: ", platform$name)
    } else {
      logger::log_warn("No columns matched standard fields for platform: ", platform$name)
    }

    return(df)
  }, error = function(e) {
    logger::log_error("Error standardizing columns: ", e$message)
    return(df)
  })
}

#' Process dates in the data frame based on platform date format
#'
#' @param df Data frame to process
#' @param platform Platform configuration
#' @return Data frame with standardized date columns
#' @export
standardize_dates <- function(df, platform) {
  tryCatch({
    # Check if date column exists
    if ("date" %in% names(df)) {
      # Get the date format from platform configuration
      date_format <- platform$date_format

      # Convert date column to Date object
      if (!is.null(date_format) && !is.na(date_format) && date_format != "") {
        df$date <- as.Date(df$date, format = date_format)
      } else {
        # Try common formats if not specified
        formats <- c("%Y-%m-%d", "%Y%m%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d")

        # Sample a few dates to detect format
        sample_dates <- head(unique(df$date), 10)

        for (fmt in formats) {
          test_dates <- as.Date(sample_dates, format = fmt)
          if (sum(!is.na(test_dates)) > 0.8 * length(sample_dates)) {
            # Found a format that works for most dates
            df$date <- as.Date(df$date, format = fmt)
            logger::log_info("Detected date format: ", fmt)
            break
          }
        }
      }

      # Check if conversion was successful
      if (is.character(df$date)) {
        logger::log_warn("Failed to convert date column to Date format, keeping as string")
      }
    }

    return(df)
  }, error = function(e) {
    logger::log_error("Error standardizing dates: ", e$message)
    return(df)
  })
}

#' Process a single platform file and insert into database
#'
#' @param conn Database connection
#' @param file_path Path to the file
#' @param platform Platform configuration
#' @param file_id Original file ID (e.g., Google Drive ID)
#' @param temp_dir Temporary directory for extraction
#' @return TRUE if successful
#' @export
process_platform_file <- function(conn, file_path, platform, file_id = NULL, temp_dir = tempdir()) {
  tryCatch({
    file_name <- basename(file_path)
    logger::log_info("Processing file: ", file_name, " for platform: ", platform$name)

    # Extract if necessary
    processed_paths <- preprocess_file(file_path, platform, temp_dir)

    # If multiple files were extracted, process each one
    if (length(processed_paths) > 1) {
      results <- sapply(processed_paths, function(path) {
        process_single_file(conn, path, platform, file_id)
      })
      return(any(results))
    } else {
      return(process_single_file(conn, processed_paths, platform, file_id))
    }
  }, error = function(e) {
    logger::log_error("Error in process_platform_file: ", e$message)
    return(FALSE)
  })
}

#' Process a single file and insert into database
#'
#' @param conn Database connection
#' @param file_path Path to the file
#' @param platform Platform configuration
#' @param file_id Original file ID (e.g., Google Drive ID)
#' @return TRUE if successful
#' @export
process_single_file <- function(conn, file_path, platform, file_id = NULL) {
  tryCatch({
    file_name <- basename(file_path)

    # Read the file
    df <- read_data_file(file_path, platform)

    if (nrow(df) == 0) {
      logger::log_warn("Empty data frame from file: ", file_name)
      return(FALSE)
    }

    # Standardize column names
    df <- standardize_columns(df, platform)

    # Standardize dates
    df <- standardize_dates(df, platform)

    # Validate required columns
    required_cols <- c("date", "track_name", "artist_name", "value")
    missing_cols <- required_cols[!required_cols %in% names(df)]

    if (length(missing_cols) > 0) {
      logger::log_warn("Missing required columns: ", paste(missing_cols, collapse = ", "),
                       " in file: ", file_name)

      # Try to infer missing columns if possible
      if ("date" %in% missing_cols) {
        # Try to extract date from filename
        date_match <- regexpr("[0-9]{8}", file_name)
        if (date_match > 0) {
          date_str <- substr(file_name, date_match, date_match + 7)
          df$date <- as.Date(date_str, format = "%Y%m%d")
          logger::log_info("Inferred date from filename: ", as.character(df$date[1]))
          missing_cols <- missing_cols[missing_cols != "date"]
        }
      }

      # If still missing required columns, return
      if (length(missing_cols) > 0) {
        return(FALSE)
      }
    }

    # Add platform information
    df$platform_id <- platform$id
    df$platform_name <- platform$name

    # Add file metadata
    df$file_name <- file_name
    if (!is.null(file_id)) {
      df$file_id <- file_id
    }
    df$processed_date <- Sys.Date()

    # Insert into database
    insert_platform_data(conn, df, platform$id)

    # Update platform metadata
    update_platform_metadata(conn, platform$id, 1, nrow(df))

    logger::log_info("Successfully processed file: ", file_name,
                     ", rows: ", nrow(df), ", platform: ", platform$name)
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error in process_single_file: ", e$message)
    return(FALSE)
  })
}

#' Clean up temporary files
#'
#' @param temp_dir Temporary directory to clean
#' @return TRUE if successful
#' @export
cleanup_temp_files <- function(temp_dir) {
  tryCatch({
    if (dir.exists(temp_dir)) {
      # List all files in the temp directory
      temp_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

      # Remove files
      if (length(temp_files) > 0) {
        file.remove(temp_files)
      }

      # List all directories
      temp_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = TRUE)
      temp_dirs <- temp_dirs[temp_dirs != temp_dir]  # Exclude the main temp dir

      # Remove directories from deepest to shallowest
      if (length(temp_dirs) > 0) {
        temp_dirs <- temp_dirs[order(nchar(temp_dirs), decreasing = TRUE)]
        sapply(temp_dirs, unlink, recursive = TRUE)
      }

      logger::log_info("Cleaned up temporary files in: ", temp_dir)
    }

    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error cleaning up temporary files: ", e$message)
    return(FALSE)
  })
}
