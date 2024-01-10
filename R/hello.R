split_basic <- function(path, col_name = NULL) {
  check_install_readxl()
  check_install_tidyverse()
  data <- read_excel(path, col_name = col_name)

  words <- str_split(data[[col_name]], " ")

  for  (m in seq_len(nrow(data))) {
    w <- 1
    index <- 1
    while (w <= length(words[[m]])) {
      col <- paste("w", w, sep = "")
      data[m, col] <- words[[m]][index]
      w <- w + 1
      index <- index + 1
    }
  }
  data <- data %>% slice(-1)
  return(data)
}


colname <- function(data) {
  a <- a %>%
    mutate_all(~ replace_na(., " "))
  columns_with_slash <- a %>%
    select(, -1) %>%
    select(where(~ any(str_detect(., "/"))))
  return(colnames(columns_with_slash))
}


split_12 <- function(path, col_name = NULL) {
  check_install_readxl()
  check_install_tidyverse()
  a <- read_excel(path, col_name = col_name)

  words <- str_split(a$A, " ")
  words

  for  (m in seq_len(nrow(a))) {
    w <- 1
    index <- 1
    while (w <= length(words[[m]])) {
      col <- paste("w", w, sep = "")
      a[m, col] <- words[[m]][index]
      w <- w + 1
      index <- index + 1
    }
  }
  for (n in colname(data)) {
    b <- str_split(a[[n]], "/")
    m <- 1

    while (m <= length(b)) {
      a[[n]] <- b[[1]][m]

      conname <- paste("con", m, sep = "")
      assign(conname, a)
      m <- m + 1
    }
  }

  return(con1)
  return(con2)
}
