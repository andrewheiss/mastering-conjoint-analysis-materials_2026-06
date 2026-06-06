library(mclogit)

# Save the model in the original .qmd file and put it some place where this can load it
# model_wtp_carpool <- mclogit(
#   choice | choice_id ~ (seat + cargo + eng + price_num) * carpool,
#   data = minivans
# )
model <- readRDS("model_wtp_carpool.rds")


# All of these fields can be dropped! 
# The different {marginaleffects} functions only really need the larger slots of
# coefficients, information.matrix, contrasts, xlevels (and maybe not even all
# those? idk, this was trial and error) 
drop <- c(
  "working.residuals", "response.residuals", "deviance.residuals",
  "y", "data", "model", "linear.predictors", "fitted.values",
  "offset", "prior.weights", "weights", "s"
)
for (nm in drop) model[[nm]] <- NULL

# Save the smaller version to use with the full Shiny, just for fun
saveRDS(model, "market-simulator-app/model.rds")

# And also save it so that base64encode() and read and encode it as binary 
b64 <- base64enc::base64encode("market-simulator-app/model.rds")

# Paste all this into the .qmd file
cat("## file: model.rds", "## type: binary", b64, sep = "\n")
