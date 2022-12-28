library(tidyverse)

###################################################
### 01
###################################################
# Check your working folder path. Change the work path so that it refers to the Desktop.
# List the files and folders in that location. Return to the folder containing the project.

# current path:
getwd()

# set path to Desktop:
setwd('C:/Users/User/Desktop')
getwd()

list.files()

# set path back to initial:
setwd('C:/Users/User/OneDrive/Edu/R/Podyplomowe R/R')
list.files()
getwd()


###################################################
### 02
###################################################
# From https://gist.githubusercontent.com/seankross/a412dfbd88b3db70b74b/raw/5f23f993cd87c283ce766e7ac6b329ee7cc2e1d1/mtcars.csv
# read columns:
#       model - as a string of characters
#       mpg - as a floating point number
#       cyl - as a floating point number
#       hp - as an integer
#       carb - as a string of characters
# Skip the other columns. Then add column "cyl6" which is TRUE if cyl==6 and FALSE otherwise
# Save the received tibble to a file.

f = "https://gist.githubusercontent.com/seankross/a412dfbd88b3db70b74b/raw/5f23f993cd87c283ce766e7ac6b329ee7cc2e1d1/mtcars.csv"

d = read_csv(f, col_types = cols_only(model = col_character(), 
                                      mpg = col_double(), 
                                      cyl = col_double(), 
                                      hp = col_integer(), 
                                      carb = col_character()))

# For short files we can specify col types as string (when we know column position):
d1 = read_csv(f, col_types = "cdd_i______c")
d1

d$cyl6 = (d$cyl == 6)
d

write_csv(d, "mtcars2.csv")

###################################################
### 03
###################################################

# Calculate:
#     - average weight
#     - average fuel consumption in liters per 100 kilometers
# for cars with at least 6 cylinders.

library(dplyr)
?mtcars  

data01 <- mtcars %>%
  tibble::rownames_to_column('car_model') %>%
  tibble::as_tibble()

# 1 mile = 1.609344 km
# 1 gallon = 3.78541178 l

# 1 mpg = 1.609344 km / g = 0.4251437 km / l
# (1 / 0.4251437) * 100  = l / km
# 235.2146 l / 100 km

1.609344 / 3.78541178
(1 / 0.4251437) * 100

data01 %>%
  filter(cyl >= 6) %>%
  mutate(lp100 = 235.2146 / mpg ) %>%
  summarise(
    mean_weight = mean(wt),
    mean_lp100 = mean(lp100), 
  )



###################################################
### 04
###################################################

# Add flag columns (TRUE/FALSE) to the iris dataset:
#   - petal length of the individual is greater than the mean + std dev. for the species
#   - petal width of the individual is greater than the mean + std dev. for the species
#   - sepal length of the individual is greater than the mean + std dev. for the species
#   - the sepal width of the individual is greater than the mean + std dev. for the species
#   - the total number of deviations of a given individual (i.e. how many of the above are TRUE)
# Sort result descending by. total number of deviations.

?iris

iris %>% tibble::as_tibble() %>%
  
  group_by(Species) %>%
  
  mutate(big_sl = (Sepal.Length > mean(Sepal.Length) + sd(Sepal.Length)),
         big_sw = (Sepal.Width > mean(Sepal.Width) + sd(Sepal.Width)),
         big_pl = (Petal.Length > mean(Petal.Length) + sd(Petal.Length)),
         big_pw = (Petal.Width > mean(Petal.Width) + sd(Petal.Width)),
         big_flower = big_pw + big_sl + big_pl + big_pw) %>%
  
  arrange(desc(big_flower))


###################################################
### 05
###################################################
# Check using the cars and owners tables:
#       1. How many people own a red six- or four-cylinder car?
#       2. Which car models do not have a single owner? (color doesn't matter)

cars = mtcars %>%
  tibble::rownames_to_column('car_model') %>%
  tibble::as_tibble()

set.seed(123)
owners = tibble(model = sample(cars$car_model, size = 20, replace = T),
                color = sample(c('black', 'white', 'red'), size = 20, replace = T),
                owners_number = sample(1:5, size=20, replace=T))

# 1
left_join(cars, owners, by = c('car_model'= 'model')) %>%
  replace_na(list(color = '-',  owners_number = 0)) %>%
  filter((cyl == 6 | cyl == 4) &  grepl('red', color)) %>%
  summarise(sum(owners_number))

# 2
left_join(cars, owners, by = c('car_model'= 'model')) %>%
  replace_na(list(color = '-',  owners_number = 0)) %>%
  filter(owners_number == 0) %>%
  select(car_model)


###################################################
### 06
###################################################

# Using tidyr package transform stocks table from:
# - timestamps are in the columns
# - indexes are in the rows
# to dataframe where:
# - indexes are in the columns
# - timestamps are in the rows.
# Then calculate mean value for each index.

# Load data
load('02_tidyverse_03d_tidyr_stocks.RData')
stocks %>% View()
colnames(stocks)

stocks %>%
  pivot_longer(-index, names_to = 'timestamp', values_to = 'value') %>%
  pivot_wider(names_from = 'index', values_from = 'value') %>%
  summarise(
    mean_cac  = mean(CAC),
    mean_dax  = mean(DAX),
    mean_ftse = mean(FTSE),
    mean_smi  = mean(SMI)
  )


stocks %>%
  pivot_longer(2:1861,
               names_to = 'Index',
               values_to = 'Index_value') %>% 
  group_by(index) %>%
  summarise(index1 = mean(Index_value)) %>% 
  pivot_wider(names_from = 'index',
              values_from = 'index1')


stocks %>%
  pivot_longer(-index, names_to = 'timestamp', values_to = 'value') %>%
  pivot_wider(names_from = 'index', values_from = 'value') %>%
  summarise_at(2:5, mean)


stocks %>%
  pivot_longer(-index, names_to = 'timestamp', values_to = 'value') %>%
  pivot_wider(names_from = 'index', values_from = 'value') %>%
  summarise(across(2:5, mean))



###################################################
### 07
###################################################

# Using who data set check if % of women with diagnosed tuberculosis
# has changed after year 2000?

?who

library(stringr)

who_clean <- who %>%
  pivot_longer(starts_with('new'), names_to = 'code', values_to = 'new_cases') %>%
  # replace NA values with 0:
  mutate_all(~replace(., is.na(.), 0)) %>%
  # add diagnosis and gender column based on code:
  mutate(diagnosis = str_extract(code, '(rel|sn|sp|ep)'),
         gender = str_extract(code, '(m|f)'),
         # extract 2 to 4 digits at the end of the code column:
         age = str_extract(code, '\\d{2,4}$'),
         age = case_when(age == '014' ~ '0-14',
                         age == '1524' ~ '15-24',
                         age == '2534' ~ '25-34',
                         age == '3544' ~ '35-44',
                         age == '4554' ~ '45-54',
                         age == '5564' ~ '55-64',
                         age == '65' ~ '65+'),
        diagnosis = factor(diagnosis),
        gender = factor(gender),
        # ordered factor:
        age = factor(age,
                     ordered = TRUE,
                     levels = c('0-14','15-24','25-34','35-44','45-54','55-64','65+')
                    )
        ) 

who_clean %>%
  mutate(after_2000 = year >2000,
         after_2000 = case_when(
            after_2000 == FALSE ~ 'before 2000',
            after_2000 == TRUE ~ 'after 2000')
         ) %>%
  group_by(gender,after_2000) %>%
  summarise(sum_of_cases = sum(new_cases)) %>%
  ungroup() %>%
  pivot_wider(values_from = 'sum_of_cases', names_from = 'gender') %>%
  mutate(Females_ratio = f / (f+m)) %>%
  rename(Females = f,
          Males = m)