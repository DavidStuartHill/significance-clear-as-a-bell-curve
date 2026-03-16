# New Chopin -1,0,1 (with boundary-hit counts)

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
        
        # Boundary hit: record, count, reset
        if (position == reset_position_high || position == reset_position_low) {
          history <- c(history, position)
          
          total_hits <- total_hits + 1
          if (position == reset_position_low)  low_hits  <- low_hits + 1
          if (position == reset_position_high) high_hits <- high_hits + 1
          
          position <- start
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

# --- Parameters for Chopin ---
steps <- 774

start_positions <- c(-5:-1, 0, 1:5, -6, 6)

# Updated boundaries
reset_position_low  <- -37
reset_position_high <- 40

num_runs <- ifelse(abs(start_positions) == 6, 50, 100)

# --- Run ---
result_chopin <- random_walk_count_multiple_starts(
  steps, start_positions, reset_position_low, reset_position_high, num_runs
)

# Histogram
walk_counts_df_aggregated <- as.data.frame(result_chopin$position_counts)
colnames(walk_counts_df_aggregated) <- c("Position", "Count")
print(walk_counts_df_aggregated)

# Boundary-hit counts
print(result_chopin$boundary_hits)