with(Shostakovich_E_X_Random_Walk_for_R_2,
     c(
       R2 = 1 - sum((Actual - `Random Walk`)^2) /
         sum((Actual - mean(Actual))^2),
       RMSE = sqrt(mean((Actual - `Random Walk`)^2))
     )
)

