P8105 Homework 5: Iteration
================
Catherine Lucey (cml2270)
11/11/2021

# Setup

Only done once, the setup code loads the necessary libraries and double
checks that the working directory is correct. It also sets defaults for
figure size with knitr, as well as a default theme and color scale for
all ggplots.

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.1 ─

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)
getwd()
```

    ## [1] "/Users/yitian/Desktop/P8105-HW5-yz4185"

``` r
set.seed(1)

knitr::opts_chunk$set(
  fig.width = 10,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

# Problem 1: Washington Post Unsolved Homicides Data

#### Installing and Describing the WaPo Homicides Data

DO THE REMOVE WITHIN THE READ CSV CALL

COUNT(VARIABLE NAME)

``` r
urlfile = "https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"

hom_df = 
  read_csv(url(urlfile)) %>% 
   mutate(city_state = str_c(city, ", ", state))
```

    ## Rows: 52179 Columns: 12

    ## ─ Column specification ────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The homicides dataset contains 53179 observations of 12 variables, these
variables include LITERALLY HOW DO YOU DO THIS/???

``` r
n_homs =
  hom_df %>% 
  group_by(city_state, disposition) %>%
  summarise(n_hom_type = n()) %>% 
  mutate(n_hom_type = as.numeric(n_hom_type)) %>% 
  pivot_wider(names_from = disposition,
              values_from = n_hom_type
  ) %>% 
  mutate(`Closed without arrest` = replace_na(`Closed without arrest`, 0)) %>% 
  mutate(`Open/No arrest` = replace_na(`Open/No arrest`, 0)) %>% 
  mutate(n_total_homs = sum(`Closed by arrest`, `Closed without arrest` , `Open/No arrest`)) %>% 
  mutate(n_unsolved = sum(`Closed without arrest`, `Open/No arrest`)) %>% 
  select(city_state, n_total_homs, n_unsolved) %>%
  filter(city_state != "Tulsa, AL")
```

    ## `summarise()` has grouped output by 'city_state'. You can override using the `.groups` argument.

#### Proportion Unsolved Homicides in Baltimore, MD

``` r
bmore_unsolved =
  n_homs %>% 
  filter(city_state == "Baltimore, MD")

prop_results = prop.test(x = bmore_unsolved$n_unsolved, n = bmore_unsolved$n_total_homs, alternative = "two.sided", conf.level = 0.95, correct = FALSE) %>% 
  broom::tidy() %>% 
  mutate(conf_int = str_c(round((conf.low*100), 2), ", ", round((conf.high*100), 2))) %>% 
  select(estimate, conf_int)
```

#### Writing a Function to Estimate Proportion Unsolved Murders in All Cities

``` r
city_prop_unsolved = function(cityname) {
  city_unsolved =
    n_homs %>% 
    filter(city_state == cityname)
  
  prop_results =
    prop.test(x = city_unsolved %>% pull(n_unsolved), 
              n = city_unsolved %>% pull(n_total_homs), 
              alternative = "two.sided", 
              conf.level = 0.95, 
              correct = FALSE) %>% 
  broom::tidy() %>% 
  select(estimate, conf.low, conf.high) %>% 
    mutate(city_name = cityname) %>% 
    as.tibble()
  return(prop_results)
}

# test the function!
city_prop_unsolved(cityname = "Atlanta, GA")
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## # A tibble: 1 × 4
    ##   estimate conf.low conf.high city_name  
    ##      <dbl>    <dbl>     <dbl> <chr>      
    ## 1    0.383    0.353     0.414 Atlanta, GA

#### Calculating the Proportion of Unsolved Homicides with CIs for All Cities

``` r
all_cities_props = map(n_homs$city_state, city_prop_unsolved) %>% 
  bind_rows()
```

#### Plotting Estimated Proportions of Unsolved Homicides in U.S. Cities

``` r
all_cities_props %>% 
  mutate(city_name = fct_reorder(city_name, estimate)) %>% 
  ggplot(aes(x = city_name, y = estimate)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high))
```

<img src="Untitled_files/figure-gfm/plotting homs data-1.png" width="90%" />

# Problem 2: Trial Data

This zip file contains data from a longitudinal study that included a
control arm and an experimental arm. Data for each participant is
included in a separate file, and file names include the subject ID and
arm.

Create a tidy dataframe containing data from all participants, including
the subject ID, arm, and observations over time:

    Start with a dataframe containing all file names; the list.files function will help
    Iterate over file names and read in data for each subject using purrr::map and saving the result as a new variable in the dataframe
    Tidy the result; manipulate file names to include control arm and subject ID, make sure weekly observations are “tidy”, and do any other tidying that’s necessary

Make a spaghetti plot showing observations on each subject over time,
and comment on differences between groups.

THIS IS SUPER WRONG

``` r
file_names_df =
  tibble(
    file_name = list.files("data")
  )

output_df = map_df(file_names_df, read.csv)
```

    ## Error in file(file, "rt"): 'description'参数不对

``` r
read_trialv6 = function(file_name){
  path = str_c("'", "data/", file_name, "'")
  read.csv(path)
}

output_df = map_df(file_names_df, read_trialv6)
```

    ## Warning in file(file, "rt"): 无法打开文件''data/'': No such file or directory

    ## Error in file(file, "rt"): 无法打开链结

``` r
test1 = read.csv('data/con_01.csv')

read_trial = function(x){
  path = str_c("./data/", x)
}

read_trialv3 = function(file_name){
  path = str_c("'", "data/", file_name, "'")
  read.csv(path)
}

fxn_output = map(file_names_df, read_trialv3)

output_df = map(file_names_df, read_trialv3)

read_trialv3(file_names_df)

read_trialv2 = function(file_name){
  read.csv2(file_name)
}

read_trialv4 = function(path){
  read.csv(path)
  return()
}

read_trialv5 = function(file_name){
  path = paste("data/", file_name,".csv", sep = "")
  read.csv(path)
}

output_df = map(file_names_df, read_trialv5)
  
  
 # output_df = read.csv()




test = read_trial(file_names_df[[1]])

CODE FROM THE INTERNET:
path<-paste(directory,"/", file_name,".csv",sep="")



for (file_name in file_names_df) {
  output_df = 
      read_trialv3(file_name)
}

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}


file_names_df %>% 
  read_trial(x = file_name)

  

read_data = map(file_names_df, read_trial)
  
file_names_df %>% 
  filter(file_name == "con_01.csv") %>%
  mutate(path = str_c("./data/", file_name)) %>% 
  read.csv("path")
```

    ## Error: <text>:43:6: unexpected symbol
    ## 42: 
    ## 43: CODE FROM
    ##          ^

``` r
path = str_c("./data/", file_names_df[[1]]) %>% 
  return(path)
```

# Problem 3: Iteratively Fixing Missing Values in Iris Data

#### Loading the Data and Introducing Missing Values

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species)) %>% 
  janitor::clean_names()
```

#### Fixing the Dataset

There are two cases to address:

    For numeric variables, you should fill in missing values with the mean of non-missing values
    For character variables, you should fill in missing values with "virginica"

Write a function that takes a vector as an argument; replaces missing
values using the rules defined above; and returns the resulting vector.
Apply this function to the columns of iris\_with\_missing using a map
statement.

``` r
add_missing = function(x){
  
  if (is.double(x)) {
    avg = round(mean(x, na.rm = TRUE), digits = 2)
    x = replace_na(x, avg)
    return(x)
  }
  
  if (is.character(x)) {
    x = replace_na(x, "virginica")
    return(x)
  }
}

replaced_df = map_df(iris_with_missing, add_missing)
```
