# ------------------------------------------------------------
# LCN / LCG generator (Scriabin, Op. 11)
# - Means: -6..6
# - Reps: 50 at -6 and +6; 100 at -5..+5
# - SD: drawn a priori from 7..15 for EACH normal
# - Truncate by rejection to [-40, 41]
# - Sample EXACTLY n_per_normal accepted draws per normal
# - Round to nearest integer pitch, bin to -40..41
# - Output: 3-column table (index, pitch, incidence)
# ------------------------------------------------------------

make_scriabin_lcn_table_big <- function(
    prelude_id = 1,
    base_seed = 1000,
    L = -40, U = 41,
    n_per_normal = 495   # 1200 * 495 = 594,000 total pitches
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
      
      # Scriabin SD draw (a priori)
      sd <- sample(7:15, 1)
      
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
tab1 <- make_scriabin_lcn_table_big(prelude_id = 1)

sum(tab1$incidence)   # should be exactly 594000
tab1

# optional save
write.csv(tab1, "Scriabin_LCN_Prelude01_pitch_table.csv", row.names = FALSE)