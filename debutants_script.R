library(worldfootballR)
library(dplyr)
library(stringr)

# Set user agent to help with connection issues
options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36")

# Define current date and week range
today <- Sys.Date()
one_week_ago <- today - 7

cat("=== DEBUG INFO ===\n")
cat(paste("Today's date:", format(today, "%Y-%m-%d"), "\n"))
cat(paste("One week ago:", format(one_week_ago, "%Y-%m-%d"), "\n"))
cat(paste("Searching for debuts between:", format(one_week_ago, "%b %d"), "-", format(today, "%b %d, %Y"), "\n\n"))

# Function to get and process debutants data with retries
get_league_debutants <- function(country_name) {
  max_retries <- 3
  attempt <- 1
  
  while(attempt <= max_retries) {
    
    tryCatch({
      # Get current year to handle season transitions
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      cat(paste("  Fetching data for years:", current_year - 1, "to", current_year, "\n"))
      
      debutants <- tm_league_debutants(
        country_name = country_name, 
        debut_type = "league", 
        debut_start_year = current_year - 1,  # Previous year for season overlap
        debut_end_year = current_year          # Current year
      )
      
      cat(paste("  Total debutants retrieved:", nrow(debutants), "\n"))
      
      if(nrow(debutants) > 0) {
        
        # Process dates with multiple format attempts
        if("debut_date" %in% colnames(debutants)) {
          debutants$date_parsed <- as.Date(debutants$debut_date, format = "%b %d, %Y")
          
          if(all(is.na(debutants$date_parsed))) {
            debutants$date_parsed <- as.Date(debutants$debut_date, format = "%Y-%m-%d")
          }
          
          # Show sample of dates for debugging
          if(nrow(debutants) > 0) {
            cat(paste("  Sample debut dates:", paste(head(debutants$debut_date, 3), collapse = ", "), "\n"))
            cat(paste("  Parsed dates:", paste(head(debutants$date_parsed, 3), collapse = ", "), "\n"))
          }
          
          if(!all(is.na(debutants$date_parsed))) {
            # Show date range in data
            cat(paste("  Date range in data:", min(debutants$date_parsed, na.rm = TRUE), "to", max(debutants$date_parsed, na.rm = TRUE), "\n"))
            
            recent_debutants <- debutants %>%
              filter(date_parsed >= one_week_ago & date_parsed <= today)
            
            cat(paste("  After date filtering:", nrow(recent_debutants), "debutants\n"))
            
            # Add country column if not present
            if(!"country" %in% names(recent_debutants)) {
              recent_debutants$country <- country_name
            }
            
            return(recent_debutants)
          } else {
            cat("  WARNING: All dates failed to parse\n")
          }
        } else {
          cat("  WARNING: No 'debut_date' column found\n")
        }
        return(NULL)
      } else {
        cat("  No debutants data returned from API\n")
        return(NULL)
      }
    }, error = function(e) {
      if(attempt == max_retries) {
        message(paste("  ERROR: Failed after", max_retries, "attempts:", e$message))
        return(NULL)
      } else {
        cat(paste("  Attempt", attempt, "failed, retrying...\n"))
        Sys.sleep(5 * attempt)
        attempt <<- attempt + 1
      }
    })
  }
}

# Function to convert age string to numeric years
convert_age_to_numeric <- function(age_str) {
  if(is.na(age_str)) return(NA)
  
  # Extract years, months, days
  years <- as.numeric(str_extract(age_str, "\\d+(?= years)"))
  months <- as.numeric(str_extract(age_str, "\\d+(?= months)"))
  days <- as.numeric(str_extract(age_str, "\\d+(?= days)"))
  
  # Handle cases where some components might be missing
  years <- ifelse(is.na(years), 0, years)
  months <- ifelse(is.na(months), 0, months)
  days <- ifelse(is.na(days), 0, days)
  
  # Convert to fractional years (approximate)
  total_years <- years + (months / 12) + (days / 365)
  return(total_years)
}

# Countries to check
countries <- c("England", "Spain", "Germany", "Italy", "France", "Portugal", "Netherlands") 

# Loop through each country
all_recent_debutants_list <- list()

cat("\n=== FETCHING DATA ===\n")
for(country in countries) {
  cat(paste("\nChecking", country, "...\n"))
  debutants_df <- get_league_debutants(country)
  
  if(!is.null(debutants_df) && nrow(debutants_df) > 0) {
    all_recent_debutants_list[[country]] <- debutants_df
    cat(paste("✓ Found", nrow(debutants_df), "recent debutant(s)\n"))
  } else {
    cat(paste("✗ No recent debutants\n"))
  }
  
  # Pause between requests
  Sys.sleep(5) 
}

# Combine and process all results
cat("\n=== PROCESSING RESULTS ===\n")
if(length(all_recent_debutants_list) > 0) {
  cat(paste("Total countries with debutants:", length(all_recent_debutants_list), "\n"))
  
  all_recent_debutants <- bind_rows(all_recent_debutants_list) %>%
    # Convert age string to numeric using our custom function
    mutate(age_debut_numeric = sapply(age_debut, convert_age_to_numeric))
  
  cat(paste("Total debutants before age filter:", nrow(all_recent_debutants), "\n"))
  
  all_recent_debutants <- all_recent_debutants %>%
    # Filter for U21 players (include NA ages in case data is missing)
    filter(age_debut_numeric < 21 | is.na(age_debut_numeric))
  
  cat(paste("Total U21 debutants:", nrow(all_recent_debutants), "\n"))
  
  all_recent_debutants <- all_recent_debutants %>%
    # Remove duplicates - using cleaned names and country
    mutate(clean_name = tolower(trimws(gsub("[^a-zA-Z]", "", player_name)))) %>%
    distinct(clean_name, country, .keep_all = TRUE) %>%
    select(-clean_name) %>%
    # Sort by debut date (newest first)
    arrange(desc(date_parsed))
  
  cat(paste("After removing duplicates:", nrow(all_recent_debutants), "\n"))
} else {
  all_recent_debutants <- data.frame()
  cat("No debutants data collected from any country\n")
}

# Generate nicely formatted output
cat("\n========================================\n")
cat("========================================\n\n")

if (nrow(all_recent_debutants) > 0) {
  # First create the email header
  cat("Weekly U21 Debutants Report (", format(one_week_ago, "%b %d"), " - ", format(today, "%b %d, %Y"), ")\n\n", sep="")
  
  # Loop through each player to create a formatted entry
  for (i in 1:nrow(all_recent_debutants)) {
    player <- all_recent_debutants[i,]
    
    # Format the player information
    cat(sprintf("Player: %s\n", player$player_name))
    cat(sprintf("Club: %s (%s)\n", player$debut_for, player$country))
    if (!is.na(player$age_debut)) {
      cat(sprintf("Age: %s\n", player$age_debut))
    }
    if (!is.na(player$date_parsed)) {
      cat(sprintf("Debut Date: %s\n", format(player$date_parsed, "%b %d, %Y")))
    }
    if (!is.na(player$player_url)) {
      cat(sprintf("Profile: %s\n\n", player$player_url))
    } else {
      cat("\n")
    }
  }
  
  cat("\n--- End of Report ---\n")
} else {
  cat("No U21 debutants found for the week of ", format(one_week_ago, "%b %d"), " - ", format(today, "%b %d, %Y"), "\n", sep="")
}
cat("\n========================================\n")
cat("========================================\n")
