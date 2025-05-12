# platforms.R
# This file contains platform definitions and configurations

#' Define music streaming platforms
#'
#' @return List of platform configurations
#' @export
define_platforms <- function() {
  list(
    # Apple Music
    apple_music = list(
      id = "am",
      name = "apple_music",
      value_type = "streams",
      file_patterns = c("^AppleMusic_Content_", "^AppleMusic_ContentDemographics_", "am-content$", "am-content-demographics$"),
      date_format = "%Y-%m-%d",
      field_map = list(
        # Primary date fields
        "Start Date" = "date",
        "Report Date" = "date",
        "Date" = "date",

        # Artist fields
        "Artist" = "artist_name",
        "Artist Name" = "artist_name",

        # Track fields
        "Title" = "track_name",
        "Track Name" = "track_name",
        "Track Title" = "track_name",

        # ID fields
        "ISRC" = "isrc",
        "Apple Identifier" = "track_id",
        "Apple ID" = "track_id",
        "Resource ID" = "track_id",
        "Track ID" = "track_id",
        "ID" = "track_id",

        # Album fields
        "Container Title" = "album_name",
        "Album" = "album_name",
        "Album Name" = "album_name",

        # Country fields
        "Storefront Name" = "country",
        "Storefront" = "country",
        "Country" = "country",
        "Territory" = "country",

        # Value fields
        "Streams" = "value",
        "Plays" = "value",
        "Stream Count" = "value",

        # Demographic fields
        "Gender" = "gender",
        "Age Band" = "age_range",
        "Age Range" = "age_range"
      )
    ),

    # iTunes
    itunes = list(
      id = "itn",
      name = "itunes",
      value_type = "purchases",
      file_patterns = c("^iTunes_Content", "^iTunes_ContentDemographics", "^iTunes_DetailedSales",
                        "itunes-content$", "itunes-content-demographics$", "itunes-detailed-sales$"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "Start Date" = "date",
        "Report Date" = "date",
        "Date" = "date",
        "Artist" = "artist_name",
        "Artist Name" = "artist_name",
        "Title" = "track_name",
        "Track Name" = "track_name",
        "ISRC" = "isrc",
        "iTunes Identifier" = "track_id",
        "Apple Identifier" = "track_id",
        "Track ID" = "track_id",
        "ID" = "track_id",
        "Container Title" = "album_name",
        "Album" = "album_name",
        "Album Name" = "album_name",
        "Country of Sale" = "country",
        "Country" = "country",
        "Quantity" = "value",
        "Units" = "value"
      )
    ),

    # Spotify
    spotify = list(
      id = "spo",
      name = "spotify",
      value_type = "streams",
      file_patterns = c("_TOPD_MLN_W_"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "Position" = "rank",
        "Date" = "date",
        "Track Name" = "track_name",
        "Artist" = "artist_name",
        "Artist Name" = "artist_name",
        "Streams" = "value",
        "URL" = "url",
        "ISRC" = "isrc",
        "Track ID" = "track_id",
        "ID" = "track_id",
        "Album" = "album_name",
        "Album Name" = "album_name",
        "Country" = "country",
        "Territory" = "country"
      )
    ),

    # SoundCloud
    soundcloud = list(
      id = "scu",
      name = "soundcloud",
      value_type = "plays",
      file_patterns = c("_Customerdata", "_Streamlevelreport", "_Trackinformation"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "Date" = "date",
        "Track Title" = "track_name",
        "Title" = "track_name",
        "Track Name" = "track_name",
        "Artist" = "artist_name",
        "Artist Name" = "artist_name",
        "ISRC" = "isrc",
        "Track ID" = "track_id",
        "ID" = "track_id",
        "Play Count" = "value",
        "Plays" = "value",
        "Streams" = "value",
        "Country" = "country",
        "Territory" = "country",
        "Release" = "album_name",
        "Album" = "album_name",
        "Album Name" = "album_name"
      )
    ),

    # AWA
    awa = list(
      id = "awa",
      name = "awa",
      value_type = "plays",
      file_patterns = c("awa_trends"),
      date_format = "%Y%m%d",
      field_map = list(
        "dt" = "date",
        "date" = "date",
        "artist_nm" = "artist_name",
        "artist_name" = "artist_name",
        "artist" = "artist_name",
        "track_nm" = "track_name",
        "track_name" = "track_name",
        "track" = "track_name",
        "title" = "track_name",
        "album_nm" = "album_name",
        "album_name" = "album_name",
        "album" = "album_name",
        "play_count" = "value",
        "streams" = "value",
        "plays" = "value",
        "region_code" = "country",
        "country" = "country",
        "territory" = "country",
        "sex" = "gender",
        "gender" = "gender",
        "age_range" = "age_range",
        "age" = "age_range",
        "isrc" = "isrc",
        "track_id" = "track_id",
        "id" = "track_id"
      )
    ),

    # Deezer
    deezer = list(
      id = "dzr",
      name = "deezer",
      value_type = "streams",
      file_patterns = c("deezer", "dzr"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "date" = "date",
        "artist_name" = "artist_name",
        "artist" = "artist_name",
        "track_name" = "track_name",
        "track" = "track_name",
        "title" = "track_name",
        "album_name" = "album_name",
        "album" = "album_name",
        "streams" = "value",
        "plays" = "value",
        "country" = "country",
        "territory" = "country",
        "isrc" = "isrc",
        "track_id" = "track_id",
        "id" = "track_id"
      )
    ),

    # Facebook
    facebook = list(
      id = "fbk",
      name = "facebook",
      value_type = "views",
      file_patterns = c("Facebook_Daily"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "date" = "date",
        "artist_name" = "artist_name",
        "artist" = "artist_name",
        "track_name" = "track_name",
        "track" = "track_name",
        "title" = "track_name",
        "album_title" = "album_name",
        "album_name" = "album_name",
        "album" = "album_name",
        "views" = "value",
        "plays" = "value",
        "streams" = "value",
        "country_code" = "country",
        "country" = "country",
        "territory" = "country",
        "isrc" = "isrc",
        "track_id" = "track_id",
        "id" = "track_id"
      )
    ),

    # Vevo
    vevo = list(
      id = "vvo",
      name = "vevo",
      value_type = "views",
      file_patterns = c("vevo"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "date" = "date",
        "artist" = "artist_name",
        "artist_name" = "artist_name",
        "title" = "track_name",
        "track" = "track_name",
        "track_name" = "track_name",
        "album" = "album_name",
        "album_name" = "album_name",
        "views" = "value",
        "plays" = "value",
        "streams" = "value",
        "country" = "country",
        "territory" = "country",
        "isrc" = "isrc",
        "track_id" = "track_id",
        "id" = "track_id",
        "video_id" = "track_id"
      )
    ),

    # Boomplay
    boomplay = list(
      id = "boo",
      name = "boomplay",
      value_type = "plays",
      file_patterns = c("Boomplay"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "date" = "date",
        "artist" = "artist_name",
        "artist_name" = "artist_name",
        "track" = "track_name",
        "track_name" = "track_name",
        "title" = "track_name",
        "album" = "album_name",
        "album_name" = "album_name",
        "plays" = "value",
        "streams" = "value",
        "views" = "value",
        "country" = "country",
        "territory" = "country",
        "isrc" = "isrc",
        "track_id" = "track_id",
        "id" = "track_id"
      )
    ),

    # Peloton
    peloton = list(
      id = "plt",
      name = "peloton",
      value_type = "plays",
      file_patterns = c("Peloton"),
      date_format = "%Y-%m-%d",
      field_map = list(
        "date" = "date",
        "artist" = "artist_name",
        "artist_name" = "artist_name",
        "track" = "track_name",
        "track_name" = "track_name",
        "title" = "track_name",
        "album" = "album_name",
        "album_name" = "album_name",
        "plays" = "value",
        "streams" = "value",
        "views" = "value",
        "country" = "country",
        "territory" = "country",
        "isrc" = "isrc",
        "track_id" = "track_id",
        "id" = "track_id"
      )
    )
  )
}

#' Match a file to its platform based on filename patterns with strict matching option
#'
#' @param file_name The name of the file to match
#' @param platforms List of platform configurations
#' @param strict Whether to strictly match only the exact file patterns (default: TRUE)
#' @return The matching platform configuration or NULL if no match
#' @export
match_file_to_platform <- function(file_name, platforms = NULL, strict = TRUE) {
  if (is.null(platforms)) {
    platforms <- define_platforms()
  }

  # First try to match based on file patterns
  for (platform_name in names(platforms)) {
    platform <- platforms[[platform_name]]
    for (pattern in platform$file_patterns) {
      if (strict) {
        # Use word boundary matching for more precise pattern matches
        if (grepl(paste0("\\b", pattern, "\\b"), file_name, ignore.case = TRUE)) {
          logger::log_info("File matched pattern '", pattern, "' for platform: ", platform$name)
          return(platform)
        }
      } else {
        # Use standard pattern matching (less strict)
        if (grepl(pattern, file_name, ignore.case = TRUE)) {
          logger::log_info("File matched pattern '", pattern, "' for platform: ", platform$name)
          return(platform)
        }
      }
    }
  }

  # If no match found with patterns, try folder structure matching if not in strict mode
  if (!strict) {
    # Special handling for Apple folder structure
    if (grepl("apl-apple|202[0-9]{3}/apl", file_name, ignore.case = TRUE)) {
      # Check if it's Apple Music or iTunes
      if (grepl("am-|_am", file_name, ignore.case = TRUE)) {
        logger::log_info("File matched Apple Music by folder structure: ", file_name)
        return(platforms$apple_music)
      } else if (grepl("itn-|_itn", file_name, ignore.case = TRUE)) {
        logger::log_info("File matched iTunes by folder structure: ", file_name)
        return(platforms$itunes)
      } else {
        # Don't default to Apple Music in any case
        logger::log_warn("File is in Apple folder but doesn't match AM or ITN: ", file_name)
        return(NULL)
      }
    }
  }

  # No matches found
  logger::log_warn("No pattern match found for file: ", file_name)
  return(NULL)
}

#' Register a new platform in metadata
#'
#' @param conn Database connection
#' @param platform_id Platform ID
#' @param platform_name Platform name
#' @param value_type Type of value (streams, plays, views, purchases, etc.)
#' @return TRUE if successful
#' @export
register_platform <- function(conn, platform_id, platform_name, value_type = "unknown") {
  tryCatch({
    # Check if platform already exists
    exists <- DBI::dbGetQuery(conn, paste0(
      "SELECT COUNT(*) as count FROM platform_metadata
       WHERE platform_id = '", platform_id, "'"
    ))$count > 0

    if (!exists) {
      # Insert new platform
      sql <- paste0(
        "INSERT INTO platform_metadata
         (platform_id, platform_name, value_type, last_update, file_count, record_count)
         VALUES ('", platform_id, "', '", platform_name, "', '", value_type, "',
                 CURRENT_TIMESTAMP, 0, 0)"
      )
      DBI::dbExecute(conn, sql)
      logger::log_info("Registered new platform: ", platform_name, " (value_type: ", value_type, ")")
    } else {
      # Update value_type if platform exists
      sql <- paste0(
        "UPDATE platform_metadata SET value_type = '", value_type, "'
         WHERE platform_id = '", platform_id, "'"
      )
      DBI::dbExecute(conn, sql)
      logger::log_info("Updated value_type for platform: ", platform_name)
    }

    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error registering platform: ", e$message)
    return(FALSE)
  })
}

#' Update platform metadata after processing files
#'
#' @param conn Database connection
#' @param platform_id Platform ID
#' @param file_count Number of files processed
#' @param record_count Number of records added
#' @return TRUE if successful
#' @export
update_platform_metadata <- function(conn, platform_id, file_count = 1, record_count = 0) {
  tryCatch({
    sql <- paste0(
      "UPDATE platform_metadata SET
       last_update = CURRENT_TIMESTAMP,
       file_count = file_count + ", file_count, ",
       record_count = record_count + ", record_count, "
       WHERE platform_id = '", platform_id, "'"
    )
    DBI::dbExecute(conn, sql)

    return(TRUE)
  }, error = function(e) {
    logger::log_error("Error updating platform metadata: ", e$message)
    return(FALSE)
  })
}

#' Initialize all platforms in the database
#'
#' @param conn Database connection
#' @return TRUE if successful
#' @export
initialize_platforms <- function(conn) {
  platforms <- define_platforms()

  for (platform_name in names(platforms)) {
    platform <- platforms[[platform_name]]
    value_type <- if (!is.null(platform$value_type)) platform$value_type else "unknown"
    register_platform(conn, platform$id, platform$name, value_type)
    create_platform_table(conn, platform$id)
  }

  return(TRUE)
}
