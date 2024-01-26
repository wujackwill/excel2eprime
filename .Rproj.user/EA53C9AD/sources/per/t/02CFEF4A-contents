#' @importFrom utils data
#' @import dplyr
#' @import stringr
#' @importFrom magrittr %>%
#' @import readxl
#' @import tidyr
NULL



#' Split the basic sentence without "/"
#'
#' @param path Path to the file
#' @param col_names column names contains the experiment sentence
#'
#' @return tibble
#' @export
#'
#' @examples split_basic("D:\\personal\\excel2ePrime\\R\\basic.xlsx", "A")


split_basic <- function(path, col_names = TRUE) {

  data <- read_excel(path, col_names = col_names)

  words <- str_split(data[[col_names]], " ")

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



#' Split sentence of the 1 * 2 experiment design
#'
#' @param path Path to the file
#' @param col_names column names contains the experiment sentence
#'
#' @return tibble
#' @export
#'
#' @examples
#' split_12("D:\\personal\\excel2ePrime\\R\\12.xlsx","A")
split_12 <- function(path, col_names = TRUE) {
  a <- read_excel(path, col_names = col_names)

  words <- str_split(a[[col_names]], " ")

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
    line1 <- line1 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line1 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]

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

    line2 <- line2 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line2 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)


    col1 <- colnames_contain[2]

    control1 <- str_split(line2[[col1]][1], "/")

    # Create a copy of the slice
    line2_copy <- line2

    line2_copy[[col1]][1] <- control1[[1]][2]

    # Store the result in the list
    line2_copy_list[[m]] <- line2_copy

    m <- m + 1
  }
  con2 <- bind_rows(line2_copy_list)
  return(list(con1 = con1, con2 = con2))
}




#' Split sentence of the 2 * 2 experiment design
#'
#' @param path Path to the file
#' @param col_names column names contains the experiment sentence
#'
#' @return tibble
#' @export
#'
#' @examples
#' split_22("D:\\personal\\excel2ePrime\\R\\22.xlsx","A")
split_22 <- function(path, col_names = TRUE) {

  a <- read_excel(path, col_names = col_names)

  words <- str_split(a[[col_names]], " ")

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

    line1 <- line1 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line1 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]

    control1 <- str_split(line1[[col1]][1], "/")
    control2 <- str_split(line1[[col2]][1], "/")

    # Create a copy of the slice
    line1_copy <- line1

    line1_copy[[col1]][1] <- control1[[1]][1]
    line1_copy[[col2]][1] <- control2[[1]][1]

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


    line2 <- line2 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line2 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]

    control1 <- str_split(line2[[col1]][1], "/")
    control2 <- str_split(line2[[col2]][1], "/")

    # Create a copy of the slice
    line2_copy <- line2

    line2_copy[[col1]][1] <- control1[[1]][1]
    line2_copy[[col2]][1] <- control2[[1]][2]

    # Store the result in the list
    line2_copy_list[[m]] <- line2_copy

    m <- m + 1
  }
  con2 <- bind_rows(line2_copy_list)

  #-----------------------------------------------

  c <- 1
  line3_copy_list <- list()

  while (c <= nrow(a)) {
    line3 <- a %>% slice(c)

    line3 <- line3 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line3 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]

    control1 <- str_split(line3[[col1]][1], "/")
    control2 <- str_split(line3[[col2]][1], "/")

    # Create a copy of the slice
    line3_copy <- line3

    line3_copy[[col1]][1] <- control1[[1]][2]
    line3_copy[[col2]][1] <- control2[[1]][1]

    # Store the result in the list
    line3_copy_list[[c]] <- line3_copy

    c <- c + 1
  }

  con3 <- bind_rows(line3_copy_list)



  #------------------------------

  d <- 1
  line4_copy_list <- list()

  while (d <= nrow(a)) {
    line4 <- a %>% slice(d)


    line4 <- line4 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line4 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]

    control1 <- str_split(line4[[col1]][1], "/")
    control2 <- str_split(line4[[col2]][1], "/")

    # Create a copy of the slice
    line4_copy <- line4

    line4_copy[[col1]][1] <- control1[[1]][2]
    line4_copy[[col2]][1] <- control2[[1]][2]

    # Store the result in the list
    line4_copy_list[[d]] <- line4_copy

    d <- d + 1
  }

  con4 <- bind_rows(line4_copy_list)
  return(list(con1 = con1, con2 = con2, con3 = con3, con4 = con4))
}





#' Split sentence of the 2 * 2 * 2 experiment design
#'
#' @param path Path to the file
#' @param col_names column names contains the experiment sentence
#'
#' @return tibble
#' @export
#'
#' @examples
#' split_222("D:\\personal\\excel2ePrime\\R\\222.xlsx","A")
split_222 <- function(path, col_names = TRUE) {

  a <- read_excel(path, col_names = col_names)

  words <- str_split(a[[col_names]], " ")

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

    line1 <- line1 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line1 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line1[[col1]][1], "/")
    control2 <- str_split(line1[[col2]][1], "/")
    control3 <- str_split(line1[[col3]][1], "/")

    # Create a copy of the slice
    line1_copy <- line1

    line1_copy[[col1]][1] <- control1[[1]][1]
    line1_copy[[col2]][1] <- control2[[1]][1]
    line1_copy[[col3]][1] <- control3[[1]][1]

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

    line2 <- line2 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line2 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line2[[col1]][1], "/")
    control2 <- str_split(line2[[col2]][1], "/")
    control3 <- str_split(line2[[col3]][1], "/")

    # Create a copy of the slice
    line2_copy <- line2

    line2_copy[[col1]][1] <- control1[[1]][1]
    line2_copy[[col2]][1] <- control2[[1]][2]
    line2_copy[[col3]][1] <- control3[[1]][2]

    # Store the result in the list
    line2_copy_list[[m]] <- line2_copy

    m <- m + 1
  }
  con3 <- bind_rows(line2_copy_list)

  #-----------------------------------------------

  c <- 1
  line3_copy_list <- list()

  while (c <= nrow(a)) {
    line3 <- a %>% slice(c)

    line3 <- line3 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line3 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line3[[col1]][1], "/")
    control2 <- str_split(line3[[col2]][1], "/")
    control3 <- str_split(line3[[col3]][1], "/")

    # Create a copy of the slice
    line3_copy <- line3

    line3_copy[[col1]][1] <- control1[[1]][2]
    line3_copy[[col2]][1] <- control2[[1]][1]
    line3_copy[[col3]][1] <- control3[[1]][1]

    # Store the result in the list
    line3_copy_list[[c]] <- line3_copy

    c <- c + 1
  }

  con5 <- bind_rows(line3_copy_list)



  #------------------------------

  d <- 1
  line4_copy_list <- list()

  while (d <= nrow(a)) {
    line4 <- a %>% slice(d)

    line4 <- line4 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line4 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line4[[col1]][1], "/")
    control2 <- str_split(line4[[col2]][1], "/")
    control3 <- str_split(line4[[col3]][1], "/")

    # Create a copy of the slice
    line4_copy <- line4

    line4_copy[[col1]][1] <- control1[[1]][2]
    line4_copy[[col2]][1] <- control2[[1]][2]
    line4_copy[[col3]][1] <- control3[[1]][2]

    # Store the result in the list
    line4_copy_list[[d]] <- line4_copy

    d <- d + 1
  }

  con8 <- bind_rows(line4_copy_list)

  #--------------------------------------
  e <- 1
  line5_copy_list <- list()

  while (e <= nrow(a)) {
    line5 <- a %>% slice(e)

    line5 <- line5 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line5 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line5[[col1]][1], "/")
    control2 <- str_split(line5[[col2]][1], "/")
    control3 <- str_split(line5[[col3]][1], "/")

    # Create a copy of the slice
    line5_copy <- line5

    line5_copy[[col1]][1] <- control1[[1]][1]
    line5_copy[[col2]][1] <- control2[[1]][2]
    line5_copy[[col3]][1] <- control3[[1]][1]

    # Store the result in the list
    line5_copy_list[[e]] <- line5_copy

    e <- e + 1
  }

  con2 <- bind_rows(line5_copy_list)

  #--------------------------------------
  f <- 1
  line6_copy_list <- list()

  while (f <= nrow(a)) {
    line6 <- a %>% slice(f)


    line6 <- line6 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line6 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line6[[col1]][1], "/")
    control2 <- str_split(line6[[col2]][1], "/")
    control3 <- str_split(line6[[col3]][1], "/")

    # Create a copy of the slice
    line6_copy <- line6

    line6_copy[[col1]][1] <- control1[[1]][1]
    line6_copy[[col2]][1] <- control2[[1]][1]
    line6_copy[[col3]][1] <- control3[[1]][2]

    # Store the result in the list
    line6_copy_list[[f]] <- line6_copy

    f <- f + 1
  }

  con4 <- bind_rows(line6_copy_list)

  #--------------------------------------
  g <- 1
  line7_copy_list <- list()

  while (g <= nrow(a)) {
    line7 <- a %>% slice(g)

    line7 <- line7 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line7 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line7[[col1]][1], "/")
    control2 <- str_split(line7[[col2]][1], "/")
    control3 <- str_split(line7[[col3]][1], "/")

    # Create a copy of the slice
    line7_copy <- line7

    line7_copy[[col1]][1] <- control1[[1]][2]
    line7_copy[[col2]][1] <- control2[[1]][2]
    line7_copy[[col3]][1] <- control3[[1]][1]

    # Store the result in the list
    line7_copy_list[[g]] <- line7_copy

    g <- g + 1
  }

  con6 <- bind_rows(line7_copy_list)

  #--------------------------------------
  h <- 1
  line8_copy_list <- list()

  while (h <= nrow(a)) {
    line8 <- a %>% slice(h)


    line8 <- line8 %>%
      mutate_all(~ replace_na(., " "))
    columns_with_slash <- line8 %>%
      select(where(~ any(str_detect(., "/"))))
    colnames_contain <- colnames(columns_with_slash)

    col1 <- colnames_contain[2]
    col2 <- colnames_contain[3]
    col3 <- colnames_contain[4]

    control1 <- str_split(line8[[col1]][1], "/")
    control2 <- str_split(line8[[col2]][1], "/")
    control3 <- str_split(line8[[col3]][1], "/")

    # Create a copy of the slice
    line8_copy <- line8

    line8_copy[[col1]][1] <- control1[[1]][2]
    line8_copy[[col2]][1] <- control2[[1]][1]
    line8_copy[[col3]][1] <- control3[[1]][2]

    # Store the result in the list
    line8_copy_list[[h]] <- line8_copy

    h <- h + 1
  }

  con7 <- bind_rows(line8_copy_list)

  return(list(con1 = con1, con2 = con2, con3 = con3, con4 = con4, con5 = con5, con6 = con6,con7 = con7, con8 = con8))
}
