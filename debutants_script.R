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
      debut_start_year = 2024, # Consider making these dynamic if needed
      debut_end_year = 2024
    )
    
    if(nrow(debutants) > 0) {
      cat("SUCCESS! Found", nrow(debutants), "debutants in", country_name, "this season.\n")
      
      # Print column names to verify structure
      # cat("Data columns available:\n") # Keep for debugging if needed
      # print(colnames(debutants))
      
      # Process dates - with extra safety checks 
      if("debut_date" %in% colnames(debutants)) {
        # Try to parse dates - show first few to check format
        # cat("Sample debut dates (first 3):\n") # Keep for debugging if needed
        # print(head(debutants$debut_date, 3))
        
        # Try to parse dates
        debutants$date_parsed <- as.Date(debutants$debut_date, format = "%b %d, %Y")
        
        # Check if parsing worked - if not, try alternate format
        if(all(is.na(debutants$date_parsed))) {
          cat("First date format failed, trying alternative (%Y-%m-%d)...\n")
          debutants$date_parsed <- as.Date(debutants$debut_date, format = "%Y-%m-%d")
        }
        
        # If we have dates, filter for recent ones
        if(!all(is.na(debutants$date_parsed))) {
          recent_debutants <- debutants %>%
            filter(date_parsed >= one_week_ago & date_parsed <= today)
            
          cat("Found", nrow(recent_debutants), "recent debutants (last 7 days) in", country_name, "\n")
          if(nrow(recent_debutants) > 0) {
            # cat("Recent debutants:\n") # Maybe too verbose for email? Output is in the file.
            # print(recent_debutants)
          }
          return(recent_debutants)
        } else {
          cat("WARNING: Could not parse dates properly for", country_name, "\n")
          # Return the whole dataframe for this country if dates failed, but without date filtering
          # Or return NULL if you only want date-filtered results
          return(NULL) # Changed to NULL as filtering failed
        }
      } else {
        cat("WARNING: No 'debut_date' column found in the data for", country_name, "\n")
        # Return NULL as we cannot filter by date
        return(NULL)
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

# Countries to check - add more as needed
countries <- c("England", "Spain", "Germany", "Italy", "France", "Portugal", "Netherlands") 

# Loop through each country
all_recent_debutants_list <- list() # Use a list to collect data frames

for(country in countries) {
  debutants_df <- get_league_debutants(country)
  
  # If we have valid debutants, add them to our list
  if(!is.null(debutants_df) && nrow(debutants_df) > 0) {
      # Add a country column for clarity if it doesn't exist from the function
      if (!"country" %in% names(debutants_df)) {
         debutants_df$country <- country 
      }
      all_recent_debutants_list[[country]] <- debutants_df
  }
  
  # Pause between requests to be polite to the server
  cat("Pausing for 5 seconds...\n")
  Sys.sleep(5) 
}

# Combine all results at the end
all_recent_debutants <- bind_rows(all_recent_debutants_list)

# Print summary of results to the output file (which becomes the email body)
cat("\n\n=== WEEKLY DEBUTANTS REPORT ===\n")
cat("Report generated on:", as.character(today), "\n")
cat("Checked leagues in:", paste(countries, collapse=", "), "\n")
cat("Reporting period:", as.character(one_week_ago), "to", as.character(today), "\n")

if(nrow(all_recent_debutants) > 0) {
  cat("\nTotal recent debutants found:", nrow(all_recent_debutants), "\n")

  # Select and print key columns for the report
  # Adjust columns as needed based on `tm_league_debutants` output
  cols_to_show <- c("player_name", "age_at_debut", "position", "club_name", "competition_name", "debut_date", "country") 
  # Filter cols_to_show to only those that actually exist in the final dataframe
  cols_to_show <- intersect(cols_to_show, colnames(all_recent_debutants))
  
  if(length(cols_to_show) > 0) {
      cat("\n--- Recent Debutants Details ---\n")
      # Use print with max rows to avoid truncation in the log/email body
      print(all_recent_debutants[, cols_to_show, drop = FALSE], max = 9999) 
  } else {
      cat("\nNOTE: Could not find standard columns to display details.\n")
      print(all_recent_debutants, max=9999) # Print everything if standard columns missing
  }

  # Optional: Add summary stats back if desired
  # Count by country/league
  if("competition_name" %in% colnames(all_recent_debutants)) {
    cat("\n--- Debutants by League ---\n")
    league_counts <- table(all_recent_debutants$competition_name)
    print(league_counts)
  }
  
  # Count by club if available
  if("club_name" %in% colnames(all_recent_debutants)) {
    cat("\n--- Debutants by Club (Top 10) ---\n")
    club_counts <- table(all_recent_debutants$club_name)
    # Ensure we don't try to print more clubs than we found
    top_n_clubs <- min(10, length(club_counts)) 
    if (top_n_clubs > 0) {
        print(sort(club_counts, decreasing = TRUE)[1:top_n_clubs]) 
    } else {
        cat("No club data available.\n")
    }
  }
  
  # Calculate average age if available
  if("age_at_debut" %in% colnames(all_recent_debutants)) {
    avg_age <- mean(as.numeric(all_recent_debutants$age_at_debut), na.rm = TRUE)
    if (!is.na(avg_age)){
       cat("\n--- Average Age ---\n")
       cat("Average age of recent debutants:", round(avg_age, 2), "years\n")
    }
  }

} else {
  cat("\nNo recent league debutants found across the selected leagues in the past week.\n")
  cat("Consider checking the source website (Transfermarkt) or adjusting the script parameters if this seems incorrect.\n")
}

cat("\n=== END OF REPORT ===\n")
