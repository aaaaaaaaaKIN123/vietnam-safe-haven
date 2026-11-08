# Parse every released R source without fitting models or installing packages.
files <- c(list.files("src", pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
           list.files("tools", pattern = "[.]R$", full.names = TRUE))
files <- files[!grepl("WS time series", files, fixed = TRUE)]
for (path in files) parse(file = path)
cat(sprintf("Parsed %d R files successfully.\n", length(files)))
