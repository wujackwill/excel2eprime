library(readxl)
library(tidyverse)
setwd("D:\\personal\\excel2ePrime\\R")

split_12 <- function() {
  a <- read_excel("1.xlsx")

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
