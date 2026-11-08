read_yaml_file <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install it in R with install.packages('yaml').", call. = FALSE)
  }
  yaml_text <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  yaml::yaml.load(yaml_text)
}

project_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  marker <- file.path(wd, "AGENT_PROJECT_GUIDE.md")
  if (file.exists(marker)) {
    return(wd)
  }

  parent <- wd
  repeat {
    candidate <- file.path(parent, "AGENT_PROJECT_GUIDE.md")
    if (file.exists(candidate)) {
      return(parent)
    }
    next_parent <- dirname(parent)
    if (identical(next_parent, parent)) {
      stop("Cannot locate project root containing AGENT_PROJECT_GUIDE.md.", call. = FALSE)
    }
    parent <- next_parent
  }
}

project_path <- function(..., root = project_root()) {
  normalizePath(file.path(root, ...), winslash = "/", mustWork = FALSE)
}

read_model_control <- function(path = "src/models/model_control.yaml") {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    stop(sprintf("Model control file not found: %s", full_path), call. = FALSE)
  }
  read_yaml_file(full_path)
}

read_model_method_map <- function(path = "src/models/model_method_map.yaml") {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    stop(sprintf("Model method map not found: %s", full_path), call. = FALSE)
  }
  read_yaml_file(full_path)
}
