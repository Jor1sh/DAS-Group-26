library(sjPlot)
# Keep only significant variables
mod.coef.logodds<- stepwise_model %>%
  summary() %>%
  coef()
plot_model(stepwise_model, show.values = TRUE, transform = NULL,
           title = "Log-Odds (quality-good)", show.p = FALSE)
