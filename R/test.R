library(readxl)
library(tidyverse)
setwd("D:\\personal\\excel2ePrime\\R")

colname <- function(data) {
  data <- data %>%
    mutate_all(~ replace_na(., " "))
  columns_with_slash <- data %>%
    select(where(~ any(str_detect(., "/"))))
  return(colnames(columns_with_slash))
}

split_12 <- function(path, col_name = NULL) {
  a <- read_excel(path, col_name = col_name)

  words <- str_split(a$A, " ")

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
  a <- a %>% slice(-1)

  #------------------------
  n <- 1
  line1_copy_list <- list()

  while (n <= nrow(a)) {
    line1 <- a %>% slice(n)

    col1 <- colname(line1)[2]

    control1 <- str_split(line1[[col1]][1], "/")

    # Create a copy of the slice
    line1_copy <- line1

    line1_copy[[col1]][1] <- control1[[1]][1]

    # Store the result in the list
    line1_copy_list[[n]] <- line1_copy

    n <- n + 1
  }
  con1 <- bind_rows(line1_copy_list)

  con1



  #-------------------------------------------------

  m <- 1
  line2_copy_list <- list()

  while (m <= nrow(a)) {
    line2 <- a %>% slice(m)

    col1 <- colname(line2)[2]

    control1 <- str_split(line2[[col1]][1], "/")

    # Create a copy of the slice
    line2_copy <- line2

    line2_copy[[col1]][1] <- control1[[1]][1]

    # Store the result in the list
    line2_copy_list[[m]] <- line2_copy

    m <- m + 1
  }
  con2 <- bind_rows(line2_copy_list)
  return (con1)
}

split_12("1.xlsx","A")

