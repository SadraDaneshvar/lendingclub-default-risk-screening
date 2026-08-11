required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "forcats",
  "tibble",
  "ggplot2",
  "patchwork",
  "scales",
  "glmnet",
  "pROC",
  "rpart",
  "ipred",
  "ranger",
  "stargazer"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

message("R dependencies are available.")
