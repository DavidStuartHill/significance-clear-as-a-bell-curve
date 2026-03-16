make_bach_lcn_table_big <- function(
    prelude_id = 1,
    base_seed = 1000,
    L = -27, U = 21,
    sd_min = 7, sd_max = 13,
    n_per_normal = 683   # 1200 * 683 = 819,600 accepted pitches
) {
  set.seed(base_seed + prelude_id)
  
  means <- -6:6
  reps  <- ifelse(abs(means) == 6, 50L, 100L)  # totals to 1200 normals
  pitch_levels <- L:U
  
  inc <- integer(length(pitch_levels))  # counts for -27..21
  
  # helper: draw exactly n accepted samples in [L,U] from N(mu, sd)
  draw_trunc_n <- function(n, mu, sd, L, U) {
    out <- numeric(0)
    while (length(out) < n) {
      # oversample in batches; acceptance rate is high here, but keep it safe
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
      sd <- sample(sd_min:sd_max, 1)
      
      x <- draw_trunc_n(n_per_normal, mu, sd, L, U)
      p <- round(x)
      p <- p[p >= L & p <= U]
      
      tab <- tabulate(p - L + 1L, nbins = length(pitch_levels))
      inc <- inc + tab
    }
  }
  
  data.frame(
    index = seq_along(pitch_levels),
    pitch = pitch_levels,
    incidence = as.integer(inc)
  )
}

# Example:
tab1 <- make_bach_lcn_table_big(prelude_id = 1, n_per_normal = 683)
sum(tab1$incidence)  # should be very close to 1200*683 (tiny loss only if rounding hits edges)
tab1