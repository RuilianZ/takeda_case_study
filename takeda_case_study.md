takeda_case_study
================
Roxy Zhang
1/5/2022

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(exploratory)
```

    ## Package attached: exploratory v0.3.13. Most recent version available on GitHub: v0.3.16
    ## You have an OPTION to update the package by typing 'update_exploratory()'. If you do so, make sure to restart R.

    ## 
    ## Attaching package: 'exploratory'

    ## The following object is masked from 'package:readr':
    ## 
    ##     read_csv

``` r
theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

## Look at the trend of nbrx form 07/01/2017 - 06/01/2019

``` r
nbrx_df = read_excel("data/data.xlsx") %>% 
  pivot_longer(
    cols = starts_with("4"),
    names_to = "date",
    values_to = "nbrx"
  ) %>% 
  janitor::clean_names() %>% 
  select(hcp_id, date, nbrx, everything()) %>% 
  mutate(
    region = as.factor(region),
    date = as.numeric(date))

# convert the numeric date to readable format
nbrx_df$date = as.Date(nbrx_df$date, origin = "1899-12-30")
```

``` r
nbrx_df %>% 
  filter(region %in% c(3, 7)) %>% 
  group_by(date, region) %>% 
  summarize(sum_nbrx = sum(nbrx)) %>% 
  ggplot(aes(x = date, y = sum_nbrx, color = region)) +
  geom_point()
```

    ## `summarise()` has grouped output by 'date'. You can override using the `.groups` argument.

![](takeda_case_study_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
