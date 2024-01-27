
<!-- README.md is generated from README.Rmd. Please edit that file -->

# excel2eprime

<!-- badges: start -->
<!-- badges: end -->

The goal of excel2eprime is to provide a easy to split experiment
sentences by different factors.

## Installation

You can install the development version of excel2eprime from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wujackwill/excel2eprime")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(excel2eprime)
```

Split experiment materials of different design

``` r
data <- split_basic("D:\\personal\\excel2eprime\\inst\\basic.xlsx","A")

data
#> # A tibble: 2 × 10
#>   A                        w1    w2    w3    w4    w5    w6    w7    w8    w9   
#>   <chr>                    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 I may have the doc in m… I     may   have  the   doc   in    my    left  hand 
#> 2 I have the doc in my le… I     have  the   doc   in    my    left  hand  <NA>
```

``` r
data <- split_12("D:\\personal\\excel2eprime\\inst\\12.xlsx","A")

data$con1
#> # A tibble: 2 × 10
#>   A                        w1    w2    w3    w4    w5    w6    w7    w8    w9   
#>   <chr>                    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 I may have/has the doc … I     may   have  the   doc   in    my    left  "han…
#> 2 I have/has the doc in m… I     have  the   doc   in    my    left  hand  " "
```

``` r
data <- split_22("D:\\personal\\excel2eprime\\inst\\22.xlsx","A")

data$con1
#> # A tibble: 2 × 10
#>   A                        w1    w2    w3    w4    w5    w6    w7    w8    w9   
#>   <chr>                    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 I may have/has the doc … I     may   have  the   doc   in    my    left  "han…
#> 2 I have/has the doc in m… I     have  the   doc   in    my    left  hand  " "
```

``` r
data <- split_222("D:\\personal\\excel2eprime\\inst\\222.xlsx","A")

data$con1
#> # A tibble: 2 × 10
#>   A                        w1    w2    w3    w4    w5    w6    w7    w8    w9   
#>   <chr>                    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 I may have/has the doc … I     may   have  the   doc   in    my    left  "han…
#> 2 I have/has the doc in m… I     have  the   doc   in    my    left  hand  " "
```
