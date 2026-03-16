df <- Shostakovich_LCGs_for_R_squared_and_RMSE

# R-squared
R2 <- cor(df$Actual, df$LCGs)^2

# RMSE
RMSE <- sqrt(mean((df$Actual - df$LCGs)^2))

R2
RMSE