check_install_readxl <<- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    message("Installing 'readxl' package...")
    install.packages("readxl")
    message("The 'readxl' package has been installed.")
  }
}



check_file <<- function(path) {
  if (!is_string(path)) {
    stop("`path` must be a string", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("`path` does not exist: ", sQuote(path), call. = FALSE)
  }
  path
}

is_integerish <<- function(x) {
  floor(x) == x
}

is_string <<- function(x) {
  length(x) == 1 && is.character(x)
}



