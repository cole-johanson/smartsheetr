
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smartsheetr

<!-- badges: start -->
<!-- badges: end -->

smartsheetr is an extensible [smartsheet](https://www.smartsheet.com/)
API client. It utilizes the API to push/pull data frames as smartsheet
documents.

## Installation

You can install the development version of smartsheetr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cole-johanson/smartsheetr")
```

You will also need to [generate a smartsheet API
key](https://help.smartsheet.com/articles/2482389-generate-API-key) and
then add it as a global environment variable:

``` r
Sys.setenv("SMARTSHEET_API_TOKEN" = <your token>)
```

## Example

Upload a sample data frame, and read it back into R:

``` r
ss_mtcars_resp = smartsheetr::ss_write_sheet("mtcars", mtcars)
ss_mtcars_sheetid = smartsheetr::ss_sheetid(ss_mtcars_resp)
smartsheetr::ss_list_sheets() |> dplyr::filter(name == 'mtcars')
#>             id   name accessLevel
#> 1 3.320559e+14 mtcars       OWNER
#> 2 4.872863e+15 mtcars       OWNER
#>                                                                    permalink
#> 1 https://app.smartsheet.com/sheets/p3JwCW24FvJpMcVcRx93C5rprXfJp6Gr4RVwXvp1
#> 2 https://app.smartsheet.com/sheets/2XGHPPwpVR22CM23cqhgRppXx65CVxWcmvRX7371
#>              createdAt           modifiedAt
#> 1 2023-08-15T21:17:43Z 2023-08-15T21:17:43Z
#> 2 2023-08-15T21:09:18Z 2023-08-15T21:09:18Z
smartsheetr::ss_read_sheet(ss_mtcars_sheetid)
#> # A tibble: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
```
