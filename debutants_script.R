library(worldfootballR)
library(dplyr)
library(stringr)

# Set user agent to help with connection issues
options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36")

# Define current date and week range
today <- Sys.Date()
one_week_ago <- today - 7

# Function to get and process debutants data with retries
get_league_debutants <- function(country_name) {
  max_retries <- 3
  attempt <- 1
  
  while(attempt <= max_retries) {
    
    tryCatch({
      debutants <- tm_league_debutants(
        country_name = country_name,  
        debut_type = "league",  
        debut_start_year = as.numeric(format(today, "%Y")), # Use current year dynamically
        debut_end_year = as.numeric(format(today, "%Y"))   # Use current year dynamically
      )
      
      if(nrow(debutants) > 0) {
        
        # Process dates with multiple format attempts
        if("debut_date" %in% colnames(debutants)) {
          debutants$date_parsed <- as.Date(debutants$debut_date, format = "%b %d, %Y")
          
          if(all(is.na(debutants$date_parsed))) {
            debutants$date_parsed <- as.Date(debutants$debut_date, format = "%Y-%m-%d")
          }
          
          if(!all(is.na(debutants$date_parsed))) {
            recent_debutants <- debutants %>%
              filter(date_parsed >= one_week_ago & date_parsed <= today)
            
            # Add country column if not present
            if(nrow(recent_debutants) > 0 && !"country" %in% names(recent_debutants)) {
              recent_debutants$country <- country_name
            }
            
            return(recent_debutants)
          }
        }
        return(NULL) # Return NULL if date processing fails or no valid dates
      } else {
        return(NULL) # Return NULL if no debutants fetched
      }
    }, error = function(e) {
      message(sprintf("Attempt %d for %s failed: %s", attempt, country_name, e$message))
      if(attempt == max_retries) {
        message(sprintf("All attempts failed for %s.", country_name))
        return(NULL)
      } else {
        Sys.sleep(5 * attempt) # Increasing delay between retries
        attempt <<- attempt + 1
      }
    })
  }
  return(NULL) # Should be caught by loop logic, but as a fallback
}

# Function to convert age string to numeric years
convert_age_to_numeric <- function(age_str) {
  if(is.na(age_str) || age_str == "") return(NA) # Handle empty strings as well
  
  # Extract years, months, days
  # Regex updated to be more robust, e.g. handle cases with only years or only days
  years <- as.numeric(str_extract(age_str, "\\d+(?=\\s*(year|years))"))
  months <- as.numeric(str_extract(age_str, "\\d+(?=\\s*(month|months))"))
  days <- as.numeric(str_extract(age_str, "\\d+(?=\\s*(day|days))"))
  
  # Handle cases where some components might be missing
  years <- ifelse(is.na(years), 0, years)
  months <- ifelse(is.na(months), 0, months)
  days <- ifelse(is.na(days), 0, days)
  
  # Convert to fractional years (approximate)
  total_years <- years + (months / 12) + (days / 365.25) # Using 365.25 for better accuracy
  return(total_years)
}

# Countries to check
countries <- c("England", "Spain", "Germany", "Italy", "France", "Portugal", "Netherlands")  

# Loop through each country
all_recent_debutants_list <- list()

for(country in countries) {
  message(sprintf("Fetching debutants for %s...", country))
  debutants_df <- get_league_debutants(country)
  
  if(!is.null(debutants_df) && nrow(debutants_df) > 0) {
    all_recent_debutants_list[[country]] <- debutants_df
  } else {
    message(sprintf("No recent debutants found for %s or data issue.", country))
  }
  
  # Pause between requests
  Sys.sleep(runif(1, 3, 7)) # Randomized sleep to be less predictable
}

# Combine and process all results
if(length(all_recent_debutants_list) > 0) {
  all_recent_debutants <- bind_rows(all_recent_debutants_list) %>%
    # Convert age string to numeric using our custom function
    mutate(age_debut_numeric = sapply(age_debut, convert_age_to_numeric)) %>%
    # Filter for U21 players (include NA ages in case data is missing)
    filter(age_debut_numeric < 21 | is.na(age_debut_numeric)) %>%
    # Remove duplicates - using cleaned names and country
    mutate(clean_name = tolower(trimws(gsub("[^a-zA-Z0-9]", "", player_name)))) %>% # Allow numbers in names
    distinct(clean_name, country, debut_date, .keep_all = TRUE) %>% # Added debut_date for more robust distinctness
    select(-clean_name) %>%
    # Sort by debut date (newest first), then by country and name
    arrange(desc(date_parsed), country, player_name)
} else {
  all_recent_debutants <- data.frame()
}

# --- MODIFIED SECTION FOR EMAIL FORMATTING ---

if (nrow(all_recent_debutants) > 0) {
  email_body <- "Recent U21 Football Debutants Report:\n"
  email_body <- paste0(email_body, "For the week ending ", format(today, "%B %d, %Y"), "\n\n")

  for (i in 1:nrow(all_recent_debutants)) {
    player_entry <- ""
    
    # Player Name
    if ("player_name" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$player_name[i])) {
      player_entry <- paste0(player_entry, "Player: ", all_recent_debutants$player_name[i], "\n")
    }
    
    # Debut For (Club)
    if ("debut_for" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$debut_for[i])) {
      player_entry <- paste0(player_entry, "Club: ", all_recent_debutants$debut_for[i], "\n")
    }
    
    # League Country
    if ("country" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$country[i])) {
      player_entry <- paste0(player_entry, "League: ", all_recent_debutants$country[i], "\n")
    }
    
    # Age at Debut
    if ("age_debut" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$age_debut[i])) {
      player_entry <- paste0(player_entry, "Age at Debut: ", all_recent_debutants$age_debut[i], "\n")
    }
    
    # Debut Date
    if ("date_parsed" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$date_parsed[i])) {
      player_entry <- paste0(player_entry, "Debut Date: ", format(all_recent_debutants$date_parsed[i], "%b %d, %Y"), "\n")
    }
    
    # Player URL
    if ("player_url" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$player_url[i])) {
      player_entry <- paste0(player_entry, "Player Profile URL: ", all_recent_debutants$player_url[i], "\n")
    }
    
    # Competition
    if ("competition_name" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$competition_name[i])) {
        player_entry <- paste0(player_entry, "Competition: ", all_recent_debutants$competition_name[i], "\n")
    }

    # Position
    if ("position" %in% colnames(all_recent_debutants) && !is.na(all_recent_debutants$position[i])) {
        player_entry <- paste0(player_entry, "Position: ", all_recent_debutants$position[i], "\n")
    }
    
    # Append the player's formatted information to the email body
    email_body <- paste0(email_body, player_entry, "\n") # Adds a blank line between player entries
  }
  
  cat(email_body) # Output the formatted string
  
} else {
  cat(paste("No U21 debutants found in the specified leagues for the week ending", format(today, "%B %d, %Y"), "\n"))
}
