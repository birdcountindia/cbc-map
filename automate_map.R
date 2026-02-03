library(googlesheets4)
library(dplyr)

UPDATE_INTERVAL <- 60 # 10 minutes in seconds
TARGET_EMAIL <- "alenalex@ncf-india.org" # Your preferred auth account

run_sync_cycle <- function() {
  tryCatch({
    cat("\n--- Cycle Started:", as.character(Sys.time()), "---\n")
    gs4_auth(email = TARGET_EMAIL)
    
    # 1. Source the file and call the function
    source("source_data.R")
    changes_made <- update_map_data() 
    
    # 2. ONLY proceed to Git if changes_made is TRUE
    if (changes_made) {
      system('git config user.email "skimmer@birdcount.in"')
      system('git config user.name "Bird Count India"')
      
      cat("Changes detected. Proceeding to Git Push...\n")
      system("git add .") # Corrected space here
      
      commit_msg <- sprintf('git commit -m "Auto-update: %s"', Sys.time())
      system(commit_msg)
      system("git push origin main")
      cat("--- Cycle Completed Successfully ---\n")
    } else {
      cat("--- Cycle ended: No new data to push. ---\n")
    }
    
  }, error = function(e) {
    cat("ERROR in cycle:", e$message, "\n")
  })
}

cat("Starting 10-minute automation loop. Press ESC or Ctrl+C to stop.\n")
repeat {
  run_sync_cycle()
  Sys.sleep(UPDATE_INTERVAL)
}