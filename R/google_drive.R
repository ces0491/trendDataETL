# google_drive.R
# Functions for connecting to and retrieving files from Google Drive

#' Set up Google Drive authentication
#'
#' @param auth_token Path to authentication token (optional)
#' @param email Google account email (optional)
#' @return TRUE if authentication successful
#' @export
setup_gdrive_auth <- function(auth_token = NULL, email = NULL) {
  tryCatch({
    # Check if googledrive package is installed
    if (!requireNamespace("googledrive", quietly = TRUE)) {
      stop("The 'googledrive' package is required. Please install it with: install.packages('googledrive')")
    }

    if (!is.null(auth_token) && file.exists(auth_token)) {
      googledrive::drive_auth(path = auth_token)
    } else if (!is.null(email)) {
      googledrive::drive_auth(email = email)
    } else {
      # Interactive authentication
      googledrive::drive_auth()
    }

    logger::log_info("Google Drive authentication successful")
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Google Drive authentication failed: ", e$message)
    return(FALSE)
  })
}

#' List folders in a Google Drive directory
#'
#' @param parent_folder_id ID or URL of the parent folder
#' @param pattern Optional regex pattern to filter folder names
#' @return Data frame of folders with id, name, and modified time
#' @export
list_gdrive_folders <- function(parent_folder_id, pattern = NULL) {
  tryCatch({
    # Get parent folder as dribble
    parent <- googledrive::as_id(parent_folder_id)

    # List folders in the parent
    folders <- googledrive::drive_ls(
      path = parent,
      type = "folder"
    )

    # Filter by pattern if provided
    if (!is.null(pattern)) {
      folders <- folders[grepl(pattern, folders$name), ]
    }

    # Sort by name (which should be a date in most cases)
    folders <- folders[order(folders$name), ]

    logger::log_info("Listed ", nrow(folders), " folders in Google Drive")
    return(folders)
  }, error = function(e) {
    logger::log_error("Error listing Google Drive folders: ", e$message)
    return(data.frame())
  })
}

#' List files in a Google Drive folder with platform matching
#'
#' @param folder_id ID or URL of the folder
#' @param platforms List of platform configurations
#' @param pattern Optional regex pattern to filter file names
#' @param strict Whether to use strict platform matching (default: TRUE)
#' @return Data frame of files with id, name, platform, and modified time
#' @export
list_gdrive_files <- function(folder_id, platforms = NULL, pattern = NULL, strict = TRUE) {
  tryCatch({
    # Default platforms if not provided
    if (is.null(platforms)) {
      platforms <- define_platforms()
    }

    # Get folder as dribble
    folder <- googledrive::as_id(folder_id)

    # List files in the folder (excluding folders)
    files <- googledrive::drive_ls(
      path = folder,
      type = "file"
    )

    # Filter by pattern if provided
    if (!is.null(pattern)) {
      files <- files[grepl(pattern, files$name), ]
    }

    # Add platform information
    files$platform <- sapply(files$name, function(file_name) {
      platform <- match_file_to_platform(file_name, platforms, strict = strict)
      if (is.null(platform)) return(NA) else return(platform$name)
    })

    # Add platform_id information
    files$platform_id <- sapply(files$name, function(file_name) {
      platform <- match_file_to_platform(file_name, platforms, strict = strict)
      if (is.null(platform)) return(NA) else return(platform$id)
    })

    # Remove files with no platform match if strict mode
    if (strict) {
      files <- files[!is.na(files$platform), ]
    }

    logger::log_info("Listed ", nrow(files), " files in Google Drive folder")
    return(files)
  }, error = function(e) {
    logger::log_error("Error listing Google Drive files: ", e$message)
    return(data.frame())
  })
}

#' Check if a file has already been processed by comparing modification times
#'
#' @param conn Database connection
#' @param file_id Google Drive file ID
#' @param file_name File name
#' @param modified_time File modification time
#' @return TRUE if file needs processing, FALSE if already processed and up to date
#' @export
check_file_needs_processing <- function(conn, file_id, file_name, modified_time) {
  tryCatch({
    # Query the processed_files table
    query <- paste0("SELECT * FROM processed_files WHERE file_id = '", file_id, "'")
    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      # File not processed yet
      logger::log_info("File not yet processed: ", file_name)
      return(TRUE)
    } else {
      # Compare modification times
      db_modified <- result$modified_time[1]

      # Convert Google Drive time to comparable format if needed
      if (is.character(modified_time)) {
        modified_time <- as.POSIXct(modified_time)
      }

      if (is.character(db_modified)) {
        db_modified <- as.POSIXct(db_modified)
      }

      if (modified_time > db_modified) {
        logger::log_info("File modified since last processing: ", file_name)
        return(TRUE)
      } else {
        logger::log_info("File already processed and up to date: ", file_name)
        return(FALSE)
      }
    }
  }, error = function(e) {
    logger::log_error("Error checking file processing status: ", e$message)
    # If there's an error, assume we need to process the file
    return(TRUE)
  })
}

#' Download a file from Google Drive to a local temporary directory
#'
#' @param file_id Google Drive file ID
#' @param file_name Original file name (for logging)
#' @param temp_dir Temporary directory to download to (default: tempdir())
#' @return Path to the downloaded file or NULL if download failed
#' @export
download_gdrive_file <- function(file_id, file_name, temp_dir = tempdir()) {
  tryCatch({
    # Create the temp directory if it doesn't exist
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE)
    }

    # Generate a local file path
    local_path <- file.path(temp_dir, file_name)

    # Download the file
    googledrive::drive_download(
      file = googledrive::as_id(file_id),
      path = local_path,
      overwrite = TRUE
    )

    logger::log_info("Downloaded file: ", file_name, " to ", local_path)
    return(local_path)
  }, error = function(e) {
    logger::log_error("Error downloading file: ", e$message)
    return(NULL)
  })
}

#' Process all new or modified files in a single folder
#'
#' @param conn Database connection
#' @param folder_id Google Drive folder ID
#' @param platforms List of platform configurations
#' @param temp_dir Temporary directory for downloads
#' @param process_function Function to process each file
#' @return Number of files processed
#' @export
process_gdrive_folder <- function(conn, folder_id, platforms = NULL,
                                  temp_dir = tempdir(),
                                  process_function = process_platform_file) {
  tryCatch({
    # List files in the folder
    files <- list_gdrive_files(folder_id, platforms)

    if (nrow(files) == 0) {
      logger::log_info("No files found in folder")
      return(0)
    }

    files_processed <- 0

    # Process each file
    for (i in 1:nrow(files)) {
      file_row <- files[i, ]

      # Check if file needs processing
      needs_processing <- check_file_needs_processing(
        conn,
        file_row$id,
        file_row$name,
        file_row$drive_resource[[i]]$modifiedTime
      )

      if (needs_processing) {
        # Download the file
        local_path <- download_gdrive_file(file_row$id, file_row$name, temp_dir)

        if (!is.null(local_path)) {
          # Get platform configuration
          platform <- match_file_to_platform(file_row$name, platforms)

          if (!is.null(platform)) {
            # Process the file
            result <- process_function(conn, local_path, platform, file_row$id)

            if (result) {
              files_processed <- files_processed + 1

              # Update processed_files table
              update_processed_file(
                conn,
                file_row$id,
                file_row$name,
                platform$id,
                file_row$drive_resource[[i]]$modifiedTime
              )
            }
          } else {
            logger::log_warn("No platform match for file: ", file_row$name)
          }

          # Clean up downloaded file
          if (file.exists(local_path)) {
            file.remove(local_path)
          }
        }
      }
    }

    logger::log_info("Processed ", files_processed, " files out of ", nrow(files), " in folder")
    return(files_processed)
  }, error = function(e) {
    logger::log_error("Error processing Google Drive folder: ", e$message)
    return(0)
  })
}

#' Process all new or modified files across multiple date folders
#'
#' @param conn Database connection
#' @param parent_folder_id Parent folder ID containing date folders
#' @param date_pattern Regex pattern to identify date folders (default: "^[0-9]{6}$")
#' @param date_range Optional vector of length 2 with start and end dates
#' @param platforms List of platform configurations
#' @param temp_dir Temporary directory for downloads
#' @return Total number of files processed
#' @export
process_gdrive_date_folders <- function(conn, parent_folder_id,
                                        date_pattern = "^[0-9]{6}$",
                                        date_range = NULL,
                                        platforms = NULL,
                                        temp_dir = tempdir()) {
  tryCatch({
    # List folders in the parent
    folders <- list_gdrive_folders(parent_folder_id, pattern = date_pattern)

    if (nrow(folders) == 0) {
      logger::log_info("No date folders found")
      return(0)
    }

    # Filter by date range if provided
    if (!is.null(date_range) && length(date_range) == 2) {
      start_date <- as.character(date_range[1])
      end_date <- as.character(date_range[2])

      folders <- folders[folders$name >= start_date & folders$name <= end_date, ]
    }

    total_processed <- 0

    # Process each folder
    for (i in 1:nrow(folders)) {
      folder <- folders[i, ]
      logger::log_info("Processing folder: ", folder$name)

      files_processed <- process_gdrive_folder(
        conn,
        folder$id,
        platforms,
        temp_dir
      )

      total_processed <- total_processed + files_processed
    }

    logger::log_info("Total files processed across all folders: ", total_processed)
    return(total_processed)
  }, error = function(e) {
    logger::log_error("Error processing Google Drive date folders: ", e$message)
    return(0)
  })
}

#' Update the processed_files table after successful processing
#'
#' @param conn Database connection
#' @param file_id Google Drive file ID
#' @param file_name File name
#' @param platform_id Platform ID
#' @param modified_time File modification time
#' @return TRUE if successful
#' @export
update_processed_file <- function(conn, file_id, file_name, platform_id, modified_time) {
  tryCatch({
    # Check if the file is already in the table
    query <- paste0("SELECT * FROM processed_files WHERE file_id = '", file_id, "'")
    result <- DBI::dbGetQuery(conn, query)

    # Format the modified time
    if (is.character(modified_time)) {
      mod_time_formatted <- modified_time
    } else {
      mod_time_formatted <- format(modified_time, "%Y-%m-%d %H:%M:%S")
    }

    if (nrow(result) == 0) {
      # Insert new record
      query <- paste0(
        "INSERT INTO processed_files
         (file_id, file_name, platform_id, processed_date, modified_time)
         VALUES ('", file_id, "', '", gsub("'", "''", file_name), "', '", platform_id, "',
                 CURRENT_TIMESTAMP, '", mod_time_formatted, "')"
      )
    } else {
      # Update existing record
      query <- paste0(
        "UPDATE processed_files SET
         file_name = '", gsub("'", "''", file_name), "',
         platform_id = '", platform_id, "',
         processed_date = CURRENT_TIMESTAMP,
         modified_time = '", mod_time_formatted, "'
         WHERE file_id = '", file_id, "'"
      )
    }

    DBI::dbExecute(conn, query)
    logger::log_info("Updated processed_files table for: ", file_name)
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error updating processed_files table: ", e$message)
    return(FALSE)
  })
}
