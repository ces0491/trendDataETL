# Load the package
library(trendDataETL)

# Define your Google Drive folder ID that contains date folders
parent_folder_id <- "1sbErepzusAytlzsL3IFhrW_fod4bwYt4"  # Example ID

# Create output directories (the functions will now do this automatically, but it's good practice)
if (!dir.exists("reports")) dir.create("reports", recursive = TRUE)
if (!dir.exists("exports")) dir.create("exports", recursive = TRUE)

# Full process with a single function call
results <- run_dsp_update(
  parent_folder_id = parent_folder_id,
  db_path = "music_streaming.db",
  output_dir = "reports"
)

# Print summary of results
print(results$summary)

# For more control, you can run the process step by step:

# Initialize the system
system <- initialize_system(
  db_type = "sqlite",
  db_path = "music_streaming.db"
)

# Authenticate with Google Drive
setup_gdrive_auth()

# Process files from the last 3 months
three_months_ago <- format(Sys.Date() - 90, "%Y%m%d")
today <- format(Sys.Date(), "%Y%m%d")

files_processed <- process_all_dsp_data(
  system,
  parent_folder_id,
  date_range = c(three_months_ago, today)
)

# Get a summary of all platform data
platform_summary <- get_platform_summary(system$conn)
print(platform_summary)

# Analyze a specific artist's trends
artist_data <- get_artist_trends(
  system$conn,
  artist_name = "21 District",
  start_date = "2024-12-01",
  end_date = "2025-01-31"
)

# Compare platforms for a specific track
platform_comparison <- get_platform_comparison(
  system$conn,
  track_name = "The Reply",
  artist_name = "21 District",
  start_date = "2024-12-01",
  end_date = "2025-01-31"
)

# Export data for a specific platform
export_platform_data(
  system$conn,
  platform_id = "spo",  # Spotify
  output_dir = "exports",
  start_date = "2024-12-01",
  end_date = "2025-01-31"
)

# Generate a report
report_file <- generate_platform_report(system, "reports")

# Close the database connection when done
DBI::dbDisconnect(system$conn)
