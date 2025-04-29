# Working Debutants Analysis Script
# This script is designed to work with confirmed column names

# Install required packages
if(!require(worldfootballR)) {
  install.packages("worldfootballR")
}
if(!require(dplyr)) {
  install.packages("dplyr")
}

# Load libraries
library(worldfootballR)
library(dplyr)

# Set user agent to help with connection issues
options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36")

# Define current date and week range
today <- Sys.Date()
one_week_ago <- today - 7
cat("Looking for debutants between", as.character(one_week_ago), "and", as.character(today), "\n")

# Function to get and process debutants data
get_league_debutants <- function(country_name) {
  cat(paste0("\nFetching data for ", country_name, "...\n"))
  
  # Fetch the data
  tryCatch({
    debutants <- tm_league_debutants(
      country_name = country_name, 
      debut_type = "league", 
      debut_start_year = 2024, 
      debut_end_year = 2024
    )
    
    if(nrow(debutants) > 0) {
      cat("SUCCESS! Found", nrow(debutants), "debutants in", country_name, "this season.\n")
      
      # Print column names to verify structure
      cat("Data columns available:\n")
      print(colnames(debutants))
      
      # Process dates - with extra safety checks 
      if("debut_date" %in% colnames(debutants)) {
        # Try to parse dates - show first few to check format
        cat("Sample debut dates (first 3):\n")
        print(head(debutants$debut_date, 3))
        
        # Try to parse dates
        debutants$date_parsed <- as.Date(debutants$debut_date, format = "%b %d, %Y")
        
        # Check if parsing worked - if not, try alternate format
        if(all(is.na(debutants$date_parsed))) {
          cat("First date format failed, trying alternative...\n")
          debutants$date_parsed <- as.Date(debutants$debut_date, format = "%Y-%m-%d")
        }
        
        # If we have dates, filter for recent ones
        if(!all(is.na(debutants$date_parsed))) {
          recent_debutants <- debutants %>%
            filter(date_parsed >= one_week_ago & date_parsed <= today)
          
          cat("Found", nrow(recent_debutants), "recent debutants (last 7 days) in", country_name, "\n")
          if(nrow(recent_debutants) > 0) {
            cat("Recent debutants:\n")
            print(recent_debutants)
          }
          return(recent_debutants)
        } else {
          cat("WARNING: Could not parse dates properly\n")
          return(debutants)
        }
      } else {
        cat("WARNING: No 'debut_date' column found in the data\n")
        return(debutants)
      }
    } else {
      cat("No debutants found for", country_name, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("ERROR fetching debutants for", country_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Countries to check - let's start with just a few
countries <- c("England", "Spain", "Germany")

# Loop through each country
all_recent_debutants <- data.frame()

for(country in countries) {
  debutants <- get_league_debutants(country)
  
  # If we have debutants, add them to our combined dataset
  if(!is.null(debutants) && nrow(debutants) > 0) {
    all_recent_debutants <- bind_rows(all_recent_debutants, debutants)
  }
  
  # Pause between requests
  Sys.sleep(3)
}

# Print summary of results
cat("\n=== SUMMARY OF RESULTS ===\n")
if(nrow(all_recent_debutants) > 0) {
  cat("Total recent debutants found:", nrow(all_recent_debutants), "\n")
  
  # Count by country/league
  if("competition_name" %in% colnames(all_recent_debutants)) {
    cat("\nDebutants by league:\n")
    league_counts <- table(all_recent_debutants$competition_name)
    print(league_counts)
  }
  
  # Count by club if available
  if("club_name" %in% colnames(all_recent_debutants)) {
    cat("\nDebutants by club:\n")
    club_counts <- table(all_recent_debutants$club_name)
    print(club_counts[order(club_counts, decreasing = TRUE)][1:10])  # Top 10 clubs
  }
  
  # Calculate average age if available
  if("age_at_debut" %in% colnames(all_recent_debutants)) {
    cat("\nAverage age of debutants:", 
        round(mean(as.numeric(all_recent_debutants$age_at_debut), na.rm = TRUE), 2), 
        "years\n")
  }
} else {
  cat("No recent debutants found across the selected leagues in the past week.\n")
  cat("Try expanding the date range or adding more leagues.\n")
}
