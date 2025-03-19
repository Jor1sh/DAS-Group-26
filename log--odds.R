library(sjPlot)
# Keep only significant variables
mod.coef.logodds<- model_new %>%
  summary() %>%
  coef()
plot_model(model_new, show.values = TRUE, transform = NULL,
           title = "Log-Odds (quality-good)", show.p = FALSE)
