library(ggplot2)

# Select numerical variables for density plotting.
numeric_vars <- sapply(data, is.numeric)
data_numeric <- data[, numeric_vars] 

# Plot the density graph
par(mfrow=c(2,2))  # 2x2 
for (var in names(data_numeric)) {
  plot(density(data_numeric[[var]]), 
       col = "blue", 
       main = paste("Density Plot of", var),
       xlab = var)
}
