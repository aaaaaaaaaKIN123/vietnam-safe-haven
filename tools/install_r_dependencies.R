# Explicit opt-in setup; run from the repository root.
packages <- c("yaml", "rugarch", "rmgarch", "MSGARCH", "tseries", "FinTS",
              "sandwich", "quantreg", "zoo", "ConnectednessApproach")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("R dependencies available.\n")
