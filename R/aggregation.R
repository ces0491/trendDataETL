# aggregation.R
# Functions for data aggregation and analysis

#' Get trending data for an artist across all platforms
#'
#' @param conn Database connection
#' @param artist_name Artist name
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return Data frame with trending data
#' @export
get_artist_trends <- function(conn, artist_name, start_date = NULL, end_date = NULL) {
  tryCatch({
    # Build SQL query
    sql <- paste0(
      "SELECT platform_id, date, SUM(value) as total_value
       FROM view_all_platforms
       WHERE artist_name = '", gsub("'", "''", artist_name), "'"
    )

    # Add date filters if provided
    if (!is.null(start_date)) {
      sql <- paste0(sql, " AND date >= '", start_date, "'")
    }
    if (!is.null(end_date)) {
      sql <- paste0(sql, " AND date <= '", end_date, "'")
    }

    # Group by platform and date
    sql <- paste0(sql, " GROUP BY platform_id, date ORDER BY platform_id, date")

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    logger::log_info("Retrieved trending data for artist: ", artist_name)
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting artist trends: ", e$message)
    return(data.frame())
  })
}

#' Get trending data for a track across all platforms
#'
#' @param conn Database connection
#' @param track_name Track name
#' @param artist_name Optional artist name to narrow results
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return Data frame with trending data
#' @export
get_track_trends <- function(conn, track_name, artist_name = NULL, start_date = NULL, end_date = NULL) {
  tryCatch({
    # Build SQL query
    sql <- paste0(
      "SELECT platform_id, date, SUM(value) as total_value
       FROM view_all_platforms
       WHERE track_name = '", gsub("'", "''", track_name), "'"
    )

    # Add artist filter if provided
    if (!is.null(artist_name)) {
      sql <- paste0(sql, " AND artist_name = '", gsub("'", "''", artist_name), "'")
    }

    # Add date filters if provided
    if (!is.null(start_date)) {
      sql <- paste0(sql, " AND date >= '", start_date, "'")
    }
    if (!is.null(end_date)) {
      sql <- paste0(sql, " AND date <= '", end_date, "'")
    }

    # Group by platform and date
    sql <- paste0(sql, " GROUP BY platform_id, date ORDER BY platform_id, date")

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    logger::log_info("Retrieved trending data for track: ", track_name)
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting track trends: ", e$message)
    return(data.frame())
  })
}

#' Get top tracks for a specific platform and time period
#'
#' @param conn Database connection
#' @param platform_id Platform ID
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param country Optional country filter
#' @param limit Number of top tracks to return (default: 50)
#' @return Data frame with top tracks
#' @export
get_top_tracks <- function(conn, platform_id, start_date, end_date, country = NULL, limit = 50) {
  tryCatch({
    # Table name
    table_name <- paste0("platform_", platform_id)

    # Build SQL query
    sql <- paste0(
      "SELECT artist_name, track_name, album_name, isrc, SUM(value) as total_value
       FROM ", table_name, "
       WHERE date >= '", start_date, "' AND date <= '", end_date, "'"
    )

    # Add country filter if provided
    if (!is.null(country)) {
      sql <- paste0(sql, " AND country = '", gsub("'", "''", country), "'")
    }

    # Group by track
    sql <- paste0(
      sql,
      " GROUP BY artist_name, track_name, album_name, isrc
        ORDER BY total_value DESC
        LIMIT ", limit
    )

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    logger::log_info("Retrieved top ", limit, " tracks for platform: ", platform_id)
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting top tracks: ", e$message)
    return(data.frame())
  })
}

#' Get platform comparison for a specific track
#'
#' @param conn Database connection
#' @param track_name Track name
#' @param artist_name Artist name
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return Data frame with platform comparison
#' @export
get_platform_comparison <- function(conn, track_name, artist_name, start_date, end_date) {
  tryCatch({
    # Build SQL query
    sql <- paste0(
      "SELECT platform_id, SUM(value) as total_value,
              COUNT(DISTINCT date) as days_with_data,
              MIN(date) as first_date,
              MAX(date) as last_date
       FROM view_all_platforms
       WHERE track_name = '", gsub("'", "''", track_name), "'
         AND artist_name = '", gsub("'", "''", artist_name), "'
         AND date >= '", start_date, "' AND date <= '", end_date, "'
       GROUP BY platform_id
       ORDER BY total_value DESC"
    )

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    # Get platform metadata
    platform_meta <- DBI::dbGetQuery(conn, "SELECT platform_id, platform_name, value_type FROM platform_metadata")

    # Join with platform metadata
    results <- merge(results, platform_meta, by = "platform_id")

    logger::log_info("Retrieved platform comparison for track: ", track_name)
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting platform comparison: ", e$message)
    return(data.frame())
  })
}

#' Get geographic distribution for a track
#'
#' @param conn Database connection
#' @param track_name Track name
#' @param artist_name Artist name
#' @param platform_id Optional platform ID filter
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return Data frame with geographic distribution
#' @export
get_geographic_distribution <- function(conn, track_name, artist_name, platform_id = NULL, start_date, end_date) {
  tryCatch({
    # Build SQL query
    sql <- paste0(
      "SELECT platform_id, country, SUM(value) as total_value
       FROM view_all_platforms
       WHERE track_name = '", gsub("'", "''", track_name), "'
         AND artist_name = '", gsub("'", "''", artist_name), "'
         AND date >= '", start_date, "' AND date <= '", end_date, "'"
    )

    # Add platform filter if provided
    if (!is.null(platform_id)) {
      sql <- paste0(sql, " AND platform_id = '", platform_id, "'")
    }

    # Group by country
    sql <- paste0(sql, " GROUP BY platform_id, country ORDER BY total_value DESC")

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    logger::log_info("Retrieved geographic distribution for track: ", track_name)
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting geographic distribution: ", e$message)
    return(data.frame())
  })
}

#' Get demographic breakdown for a track (if available)
#'
#' @param conn Database connection
#' @param track_name Track name
#' @param artist_name Artist name
#' @param platform_id Platform ID
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return Data frame with demographic breakdown
#' @export
get_demographic_breakdown <- function(conn, track_name, artist_name, platform_id, start_date, end_date) {
  tryCatch({
    # Table name
    table_name <- paste0("platform_", platform_id)

    # Check if demographic columns exist
    table_info <- DBI::dbGetQuery(conn, paste0("PRAGMA table_info(", table_name, ")"))
    has_gender <- "gender" %in% table_info$name
    has_age <- "age_range" %in% table_info$name

    if (!has_gender && !has_age) {
      logger::log_warn("No demographic data available for platform: ", platform_id)
      return(data.frame())
    }

    # Build SQL query based on available columns
    if (has_gender && has_age) {
      group_cols <- "gender, age_range"
    } else if (has_gender) {
      group_cols <- "gender"
    } else {
      group_cols <- "age_range"
    }

    sql <- paste0(
      "SELECT ", group_cols, ", SUM(value) as total_value
       FROM ", table_name, "
       WHERE track_name = '", gsub("'", "''", track_name), "'
         AND artist_name = '", gsub("'", "''", artist_name), "'
         AND date >= '", start_date, "' AND date <= '", end_date, "'"
    )

    # Group by demographic columns
    sql <- paste0(sql, " GROUP BY ", group_cols, " ORDER BY total_value DESC")

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    logger::log_info("Retrieved demographic breakdown for track: ", track_name)
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting demographic breakdown: ", e$message)
    return(data.frame())
  })
}

#' Get platform activity summary
#'
#' @param conn Database connection
#' @return Data frame with platform activity summary
#' @export
get_platform_summary <- function(conn) {
  tryCatch({
    # Get platform metadata
    platform_meta <- DBI::dbGetQuery(conn, "SELECT * FROM platform_metadata")

    # For each platform, get date range and count
    platforms <- platform_meta$platform_id

    results <- data.frame(
      platform_id = character(),
      platform_name = character(),
      value_type = character(),
      total_records = integer(),
      first_date = character(),
      last_date = character(),
      days_with_data = integer(),
      stringsAsFactors = FALSE
    )

    for (platform_id in platforms) {
      table_name <- paste0("platform_", platform_id)

      # Check if table exists
      table_exists <- DBI::dbExistsTable(conn, table_name)

      if (table_exists) {
        sql <- paste0(
          "SELECT COUNT(*) as total_records,
                 MIN(date) as first_date,
                 MAX(date) as last_date,
                 COUNT(DISTINCT date) as days_with_data
           FROM ", table_name
        )

        platform_stats <- DBI::dbGetQuery(conn, sql)

        if (nrow(platform_stats) > 0) {
          platform_row <- platform_meta[platform_meta$platform_id == platform_id, ]

          results <- rbind(results, data.frame(
            platform_id = platform_id,
            platform_name = platform_row$platform_name,
            value_type = platform_row$value_type,
            total_records = platform_stats$total_records,
            first_date = platform_stats$first_date,
            last_date = platform_stats$last_date,
            days_with_data = platform_stats$days_with_data,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    logger::log_info("Retrieved platform summary")
    return(results)
  }, error = function(e) {
    logger::log_error("Error getting platform summary: ", e$message)
    return(data.frame())
  })
}

#' Export platform data to CSV
#'
#' @param conn Database connection
#' @param platform_id Platform ID
#' @param output_dir Output directory
#' @param start_date Optional start date filter
#' @param end_date Optional end date filter
#' @return Path to the exported file
#' @export
export_platform_data <- function(conn, platform_id, output_dir = ".", start_date = NULL, end_date = NULL) {
  tryCatch({
    # Table name
    table_name <- paste0("platform_", platform_id)

    # Build SQL query
    sql <- paste0("SELECT * FROM ", table_name)

    # Add date filters if provided
    where_clauses <- c()
    if (!is.null(start_date)) {
      where_clauses <- c(where_clauses, paste0("date >= '", start_date, "'"))
    }
    if (!is.null(end_date)) {
      where_clauses <- c(where_clauses, paste0("date <= '", end_date, "'"))
    }

    if (length(where_clauses) > 0) {
      sql <- paste0(sql, " WHERE ", paste(where_clauses, collapse = " AND "))
    }

    # Order by date
    sql <- paste0(sql, " ORDER BY date, artist_name, track_name")

    # Execute query
    results <- DBI::dbGetQuery(conn, sql)

    if (nrow(results) == 0) {
      logger::log_warn("No data to export for platform: ", platform_id)
      return(NULL)
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Get platform name
    platform_meta <- DBI::dbGetQuery(
      conn,
      paste0("SELECT platform_name FROM platform_metadata WHERE platform_id = '", platform_id, "'")
    )
    platform_name <- if (nrow(platform_meta) > 0) platform_meta$platform_name[1] else platform_id

    # Create file name with date range
    if (!is.null(start_date) && !is.null(end_date)) {
      file_name <- paste0(platform_name, "_", start_date, "_to_", end_date, ".csv")
    } else {
      file_name <- paste0(platform_name, "_all_data.csv")
    }

    file_path <- file.path(output_dir, file_name)

    # Write to CSV
    utils::write.csv(results, file_path, row.names = FALSE)

    logger::log_info("Exported ", nrow(results), " rows to ", file_path)
    return(file_path)
  }, error = function(e) {
    logger::log_error("Error exporting platform data: ", e$message)
    return(NULL)
  })
}
