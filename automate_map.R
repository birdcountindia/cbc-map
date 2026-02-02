library(googlesheets4)
library(dplyr)

UPDATE_INTERVAL <- 60 # 10 minutes in seconds
TARGET_EMAIL <- "alenalex@ncf-india.org" # Your preferred auth account

run_sync_cycle <- function() {
  tryCatch({
    cat("\n--- Cycle Started:", as.character(Sys.time()), "---\n")
    gs4_auth(email = TARGET_EMAIL) #
    source("source_data.R") 
    
    system('git config user.email "skimmer@birdcount.in"')
    system('git config user.name "Bird Count India"')
    
    cat("Changes detected. Proceeding to Git Push...\n")
    system("git add.")
    commit_msg <- sprintf('git commit -m "Auto-update: %s"', Sys.time())
    system(commit_msg)
    system("git push origin main")
    cat("--- Cycle Completed Successfully ---\n")
  }, error = function(e) {
    cat("ERROR in cycle:", e$message, "\n")
  })
}

cat("Starting 10-minute automation loop. Press ESC or Ctrl+C to stop.\n")
repeat {
  run_sync_cycle()
  Sys.sleep(UPDATE_INTERVAL)
}