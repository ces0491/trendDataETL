# database.R
# Functions for database connection and operations

#' Set up database connection
#'
#' @param db_type Database type ('sqlite', 'postgres', 'mysql', etc.)
#' @param db_path Database path or connection string
#' @param db_user Username (not needed for SQLite)
#' @param db_password Password (not needed for SQLite)
#' @param db_host Host address (not needed for SQLite)
#' @param db_port Port number (not needed for SQLite)
#' @return Database connection object
#' @export
setup_database <- function(db_type = "sqlite", db_path = "music_streaming.db",
                           db_user = NULL, db_password = NULL,
                           db_host = NULL, db_port = NULL) {
  tryCatch({
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("Package 'DBI' is required. Please install it.")
    }

    conn <- NULL

    if (db_type == "sqlite") {
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("Package 'RSQLite' is required for SQLite. Please install it.")
      }
      conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      logger::log_info("Connected to SQLite database: ", db_path)
    } else if (db_type == "postgres") {
      if (!requireNamespace("RPostgres", quietly = TRUE)) {
        stop("Package 'RPostgres' is required for PostgreSQL. Please install it.")
      }
      conn <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = db_path,
        host = db_host,
        port = db_port,
        user = db_user,
        password = db_password
      )
      logger::log_info("Connected to PostgreSQL database: ", db_path)
    } else if (db_type == "mysql" || db_type == "mariadb") {
      if (!requireNamespace("RMariaDB", quietly = TRUE)) {
        stop("Package 'RMariaDB' is required for MySQL/MariaDB. Please install it.")
      }
      conn <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        dbname = db_path,
        host = db_host,
        port = db_port,
        user = db_user,
        password = db_password
      )
      logger::log_info("Connected to MySQL/MariaDB database: ", db_path)
    } else {
      stop("Unsupported database type: ", db_type)
    }

    # Initialize database tables
    initialize_database_tables(conn, db_type)

    return(conn)
  }, error = function(e) {
    logger::log_error("Database connection error: ", e$message)
    stop(e)
  })
}

#' Create necessary database tables
#'
#' @param conn Database connection
#' @param db_type Database type
#' @return TRUE if successful
#' @export
initialize_database_tables <- function(conn, db_type = "sqlite") {
  tryCatch({
    # Create processed_files table
    create_processed_files_table(conn, db_type)

    # Create platform_metadata table
    create_platform_metadata_table(conn, db_type)

    # Initialize platforms
    initialize_platforms(conn)

    logger::log_info("Database tables initialized")
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error initializing database tables: ", e$message)
    return(FALSE)
  })
}

#' Create the processed_files table
#'
#' @param conn Database connection
#' @param db_type Database type
#' @return TRUE if successful
#' @export
create_processed_files_table <- function(conn, db_type = "sqlite") {
  tryCatch({
    # Check if table exists
    table_exists <- DBI::dbExistsTable(conn, "processed_files")

    if (!table_exists) {
      # Create the table
      sql <- paste0(
        "CREATE TABLE processed_files (
           id INTEGER PRIMARY KEY ", if(db_type == "sqlite") "AUTOINCREMENT" else "AUTO_INCREMENT", ",
           file_id TEXT NOT NULL UNIQUE,
           file_name TEXT NOT NULL,
           platform_id TEXT NOT NULL,
           processed_date TIMESTAMP,
           modified_time TIMESTAMP,
           record_count INTEGER DEFAULT 0
         )"
      )
      DBI::dbExecute(conn, sql)

      # Create index on file_id
      DBI::dbExecute(conn, "CREATE INDEX idx_processed_files_file_id ON processed_files (file_id)")

      logger::log_info("Created processed_files table")
    }

    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error creating processed_files table: ", e$message)
    return(FALSE)
  })
}

#' Create the platform_metadata table
#'
#' @param conn Database connection
#' @param db_type Database type
#' @return TRUE if successful
#' @export
create_platform_metadata_table <- function(conn, db_type = "sqlite") {
  tryCatch({
    # Check if table exists
    table_exists <- DBI::dbExistsTable(conn, "platform_metadata")

    if (!table_exists) {
      # Create the table
      sql <- paste0(
        "CREATE TABLE platform_metadata (
           id INTEGER PRIMARY KEY ", if(db_type == "sqlite") "AUTOINCREMENT" else "AUTO_INCREMENT", ",
           platform_id TEXT NOT NULL UNIQUE,
           platform_name TEXT NOT NULL,
           value_type TEXT,
           last_update TIMESTAMP,
           file_count INTEGER DEFAULT 0,
           record_count INTEGER DEFAULT 0
         )"
      )
      DBI::dbExecute(conn, sql)

      # Create index on platform_id
      DBI::dbExecute(conn, "CREATE INDEX idx_platform_metadata_platform_id ON platform_metadata (platform_id)")

      logger::log_info("Created platform_metadata table")
    }

    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error creating platform_metadata table: ", e$message)
    return(FALSE)
  })
}

#' Create platform data table for a specific platform
#'
#' @param conn Database connection
#' @param platform_id Platform ID
#' @param db_type Database type
#' @return TRUE if successful
#' @export
create_platform_table <- function(conn, platform_id, db_type = "sqlite") {
  tryCatch({
    # Table name is "platform_" followed by platform_id
    table_name <- paste0("platform_", platform_id)

    # Check if table exists
    table_exists <- DBI::dbExistsTable(conn, table_name)

    if (!table_exists) {
      # Create the table
      sql <- paste0(
        "CREATE TABLE ", table_name, " (
           id INTEGER PRIMARY KEY ", if(db_type == "sqlite") "AUTOINCREMENT" else "AUTO_INCREMENT", ",
           date DATE,
           artist_name TEXT,
           track_name TEXT,
           album_name TEXT,
           isrc TEXT,
           track_id TEXT,
           country TEXT,
           value NUMERIC,
           rank INTEGER,
           gender TEXT,
           age_range TEXT,
           file_id TEXT,
           file_name TEXT,
           processed_date DATE,
           UNIQUE(date, artist_name, track_name, isrc, country)
         )"
      )
      DBI::dbExecute(conn, sql)

      # Create indexes
      DBI::dbExecute(conn, paste0("CREATE INDEX idx_", table_name, "_date ON ", table_name, " (date)"))
      DBI::dbExecute(conn, paste0("CREATE INDEX idx_", table_name, "_artist ON ", table_name, " (artist_name)"))
      DBI::dbExecute(conn, paste0("CREATE INDEX idx_", table_name, "_track ON ", table_name, " (track_name)"))
      DBI::dbExecute(conn, paste0("CREATE INDEX idx_", table_name, "_isrc ON ", table_name, " (isrc)"))
      DBI::dbExecute(conn, paste0("CREATE INDEX idx_", table_name, "_country ON ", table_name, " (country)"))

      logger::log_info("Created table: ", table_name)
    }

    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error creating platform table: ", e$message)
    return(FALSE)
  })
}

#' Insert data into platform table
#'
#' @param conn Database connection
#' @param df Data frame to insert
#' @param platform_id Platform ID
#' @return Number of rows inserted
#' @export
insert_platform_data <- function(conn, df, platform_id) {
  tryCatch({
    # Make sure the platform table exists
    create_platform_table(conn, platform_id)

    table_name <- paste0("platform_", platform_id)

    # Get the column names in the table
    table_info <- DBI::dbGetQuery(conn, paste0("PRAGMA table_info(", table_name, ")"))
    table_cols <- table_info$name

    # Filter data frame to only include columns in the table
    df_filtered <- df[, intersect(names(df), table_cols), drop = FALSE]

    # Handle missing required columns
    for (col in c("date", "artist_name", "track_name", "value")) {
      if (!col %in% names(df_filtered)) {
        df_filtered[[col]] <- NA
      }
    }

    # Insert data with UPSERT logic
    rows_inserted <- 0

    # Start transaction for better performance
    DBI::dbBegin(conn)

    # Insert data in chunks to avoid memory issues
    chunk_size <- 1000
    total_rows <- nrow(df_filtered)
    num_chunks <- ceiling(total_rows / chunk_size)

    for (i in 1:num_chunks) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, total_rows)
      chunk <- df_filtered[start_idx:end_idx, ]

      # Insert data
      for (j in 1:nrow(chunk)) {
        row <- chunk[j, ]

        # Build column names and values for SQL query
        col_names <- paste(names(row), collapse = ", ")
        values <- sapply(1:ncol(row), function(k) {
          val <- row[1, k]
          if (is.na(val)) {
            return("NULL")
          } else if (is.character(val) || is.factor(val)) {
            return(paste0("'", gsub("'", "''", as.character(val)), "'"))
          } else {
            return(as.character(val))
          }
        })
        values_str <- paste(values, collapse = ", ")

        # Build ON CONFLICT clause for UPSERT
        key_cols <- c("date", "artist_name", "track_name", "isrc", "country")
        key_cols <- intersect(key_cols, names(row))

        if (length(key_cols) > 0) {
          # Build ON CONFLICT clause
          conflict_cols <- paste(key_cols, collapse = ", ")

          update_cols <- setdiff(names(row), c(key_cols, "id"))
          if (length(update_cols) > 0) {
            update_clause <- paste(sapply(update_cols, function(col) {
              paste0(col, " = EXCLUDED.", col)
            }), collapse = ", ")

            # SQLite syntax for upsert
            sql <- paste0(
              "INSERT INTO ", table_name, " (", col_names, ")
               VALUES (", values_str, ")
               ON CONFLICT (", conflict_cols, ") DO UPDATE SET
               ", update_clause
            )
          } else {
            # No columns to update, just do INSERT OR IGNORE
            sql <- paste0(
              "INSERT OR IGNORE INTO ", table_name, " (", col_names, ")
               VALUES (", values_str, ")"
            )
          }
        } else {
          # No key columns, just insert
          sql <- paste0(
            "INSERT INTO ", table_name, " (", col_names, ")
             VALUES (", values_str, ")"
          )
        }

        # Execute the query
        result <- DBI::dbExecute(conn, sql)
        rows_inserted <- rows_inserted + result
      }
    }

    # Commit transaction
    DBI::dbCommit(conn)

    logger::log_info("Inserted ", rows_inserted, " rows into ", table_name)
    return(rows_inserted)
  }, error = function(e) {
    # Rollback transaction on error
    DBI::dbRollback(conn)
    logger::log_error("Error inserting data: ", e$message)
    return(0)
  })
}

#' Create aggregated views for efficient querying
#'
#' @param conn Database connection
#' @return TRUE if successful
#' @export
create_aggregated_views <- function(conn) {
  tryCatch({
    # Get all platform tables
    platform_data <- DBI::dbGetQuery(conn, "SELECT platform_id FROM platform_metadata")

    for (i in 1:nrow(platform_data)) {
      platform_id <- platform_data$platform_id[i]
      table_name <- paste0("platform_", platform_id)

      # Create daily aggregated view
      daily_view_name <- paste0("view_daily_", platform_id)
      daily_view_sql <- paste0(
        "CREATE VIEW IF NOT EXISTS ", daily_view_name, " AS
         SELECT
           date,
           artist_name,
           track_name,
           album_name,
           isrc,
           country,
           SUM(value) as total_value,
           AVG(rank) as avg_rank,
           COUNT(*) as count
         FROM ", table_name, "
         GROUP BY date, artist_name, track_name, isrc, country"
      )
      DBI::dbExecute(conn, daily_view_sql)

      # Create monthly aggregated view
      monthly_view_name <- paste0("view_monthly_", platform_id)
      monthly_view_sql <- paste0(
        "CREATE VIEW IF NOT EXISTS ", monthly_view_name, " AS
         SELECT
           strftime('%Y-%m', date) as month,
           artist_name,
           track_name,
           album_name,
           isrc,
           country,
           SUM(value) as total_value,
           AVG(rank) as avg_rank,
           COUNT(DISTINCT date) as unique_days
         FROM ", table_name, "
         GROUP BY month, artist_name, track_name, isrc, country"
      )
      DBI::dbExecute(conn, monthly_view_sql)
    }

    # Create all-platform aggregated view
    all_platforms_sql <- "CREATE VIEW IF NOT EXISTS view_all_platforms AS\n"

    # Union all platform tables
    for (i in 1:nrow(platform_data)) {
      platform_id <- platform_data$platform_id[i]
      table_name <- paste0("platform_", platform_id)

      all_platforms_sql <- paste0(
        all_platforms_sql,
        "SELECT
           '", platform_id, "' as platform_id,
           date,
           artist_name,
           track_name,
           album_name,
           isrc,
           country,
           value
         FROM ", table_name, "\n"
      )

      if (i < nrow(platform_data)) {
        all_platforms_sql <- paste0(all_platforms_sql, "UNION ALL\n")
      }
    }

    DBI::dbExecute(conn, all_platforms_sql)

    logger::log_info("Created aggregated views")
    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error creating aggregated views: ", e$message)
    return(FALSE)
  })
}
