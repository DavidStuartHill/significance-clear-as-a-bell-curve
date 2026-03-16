# New Bach -1,0,1 (with boundary-hit counts) — indexed from E(X)

random_walk_count_multiple_starts <- function(steps, start_positions,
                                              reset_position_low, reset_position_high,
                                              num_runs) {
  all_history <- c()
  
  total_hits <- 0
  low_hits   <- 0
  high_hits  <- 0
  
  for (start in start_positions) {
    num_run <- num_runs[start == start_positions]
    
    for (run in 1:num_run) {
      position <- start
      history  <- c(position)
      
      for (i in 1:steps) {
        step <- sample(c(-1, 0, 1), 1)
        position <- position + step
        
        # Check for boundary hit before resetting
        if (position == reset_position_high || position == reset_position_low) {
          history <- c(history, position)  # record boundary hit
          
          total_hits <- total_hits + 1
          if (position == reset_position_low)  low_hits  <- low_hits + 1
          if (position == reset_position_high) high_hits <- high_hits + 1
          
          position <- start  # reset to starting position
        } else {
          history <- c(history, position)
        }
      }
      
      all_history <- c(all_history, history)
    }
  }
  
  position_counts <- table(all_history)
  
  return(list(
    position_counts = position_counts,
    boundary_hits = list(
      total = total_hits,
      low   = low_hits,
      high  = high_hits
    )
  ))
}

# --- Parameters for Bach ---
steps <- 682

start_positions <- c(-5:-1, 0, 1:5, -6, 6)

# Updated boundaries (indexed from E(X))
reset_position_low  <- -27
reset_position_high <- 21

num_runs <- ifelse(abs(start_positions) == 6, 50, 100)

# --- Run ---
result_bach <- random_walk_count_multiple_starts(
  steps, start_positions, reset_position_low, reset_position_high, num_runs
)

# Histogram as a data frame (optional)
walk_counts_df_aggregated <- as.data.frame(result_bach$position_counts)
colnames(walk_counts_df_aggregated) <- c("Position", "Count")
print(walk_counts_df_aggregated)

# Boundary-hit counts (this is the key output)
print(result_bach$boundary_hits)