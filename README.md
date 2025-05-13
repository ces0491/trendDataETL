# README.md

# MusicStreamTrends

An R package to process, standardize, and analyze music streaming data from different Digital Service Providers (DSPs).

## Installation

Follow these steps to install the package:

### Method 1: Using devtools (Recommended)

1. Install devtools if you don't have it:
```r
install.packages("devtools")
```

2. Create a proper package structure:
```r
# Create directories
dir.create("MusicStreamTrends/R", recursive = TRUE)
dir.create("MusicStreamTrends/man", recursive = TRUE)
```

3. Copy all R files to the MusicStreamTrends/R directory:
```r
# Copy files (adjust paths as needed)
file.copy("platforms.R", "MusicStreamTrends/R/")
file.copy("google_drive.R", "MusicStreamTrends/R/")
file.copy("file_processing.R", "MusicStreamTrends/R/")
file.copy("database.R", "MusicStreamTrends/R/")
file.copy("aggregation.R", "MusicStreamTrends/R/")
file.copy("utils.R", "MusicStreamTrends/R/")
```

4. Copy DESCRIPTION and NAMESPACE files:
```r
file.copy("DESCRIPTION", "MusicStreamTrends/")
file.copy("NAMESPACE", "MusicStreamTrends/")
```

5. Build and install the package:
```r
devtools::install("MusicStreamTrends")
```

### Method 2: Manual Loading (For Testing)

If you're just testing the package functions without a formal installation:

1. Source all R files directly:
```r
source("platforms.R")
source("google_drive.R")
source("file_processing.R")
source("database.R")
source("aggregation.R")
source("utils.R")
```

## Usage

After installation:

```r
# Load the package
library(MusicStreamTrends)

# Define your Google Drive folder ID that contains date folders
parent_folder_id <- "your_folder_id_here"  # Replace with your actual ID

# Create output directories
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

# For step-by-step usage, see example.R in the package documentation
```

## Troubleshooting

If you encounter the error "could not find function", make sure:

1. You've either installed the package and loaded it with `library(MusicStreamTrends)`, or
2. You've sourced all the individual R files

## Dependencies

Make sure you have all required packages installed:

```r
install.packages(c("DBI", "RSQLite", "logger", "googledrive", "readr", "readxl"))
```
