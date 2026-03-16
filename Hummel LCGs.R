# ------------------------------------------------------------
# LCN / LCG generator (Hummel)
# - Means: -6..6
# - Reps: 50 at -6 and +6; 100 at -5..+5
# - SD: drawn a priori from 8..16 for EACH normal
# - Truncate by rejection to [-33, 36]
# - Sample EXACTLY n_per_normal accepted draws per normal
# - Round to nearest integer pitch, bin to -33..36
# - Output: 3-column table (index, pitch, incidence)
# ------------------------------------------------------------

make_hummel_lcn_table_big <- function(
    prelude_id = 1,
    base_seed = 1000,
    L = -33, U = 36,
    n_per_normal = 130   # <-- gives ~156,000 total notes
) {
  set.seed(base_seed + prelude_id)
  
  means <- -6:6
  reps  <- ifelse(abs(means) == 6, 50L, 100L)  # totals 1200 normals
  pitch_levels <- L:U
  
  inc <- integer(length(pitch_levels))
  
  draw_trunc_n <- function(n, mu, sd, L, U) {
    out <- numeric(0)
    while (length(out) < n) {
      m <- ceiling((n - length(out)) * 1.3) + 10
      x <- rnorm(m, mean = mu, sd = sd)
      x <- x[x >= L & x <= U]
      if (length(x)) out <- c(out, x)
    }
    out[1:n]
  }
  
  for (i in seq_along(means)) {
    mu <- means[i]
    for (j in seq_len(reps[i])) {
      
      # Hummel SD draw (a priori)
      sd <- sample(8:16, 1)
      
      x <- draw_trunc_n(n_per_normal, mu, sd, L, U)
      
      p <- round(x)
      p <- p[p >= L & p <= U]
      
      inc <- inc + tabulate(p - L + 1L, nbins = length(pitch_levels))
    }
  }
  
  data.frame(
    index = seq_along(pitch_levels),
    pitch = pitch_levels,
    incidence = as.integer(inc)
  )
}

# ------------------------
# RUN EXAMPLE (Prelude 1)
# ------------------------
tab1 <- make_hummel_lcn_table_big(prelude_id = 1)

sum(tab1$incidence)   # should be exactly 1200 * 130 = 156000
tab1

# optional save
write.csv(tab1, "Hummel_LCN_Prelude01_pitch_table.csv", row.names = FALSE)