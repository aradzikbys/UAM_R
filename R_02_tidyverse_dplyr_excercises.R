###################################################
### DPLYR - 01
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
### DPLYR - 02
###################################################

# Add flag columns (TRUE/FALSE) to the iris dataset:
#   - petal length of the individual is greater than the mean + std dev. for the species
#   - petal width of the individual is greater than the mean + std dev. for the species
#   - sepal length of the individual is greater than the mean + std dev. for the species
#   - the sepal width of the individual is greater than the mean + std dev. for the species
#   - the total number of deviations of a given individual (i.e. how many of the above are TRUE)
# Sort result descending by. total number of deviations.


library(dplyr)
?iris

data_iris <- as_tibble(iris)
data_iris

         
data_iris %>%
  group_by(Species) %>%
  
  mutate(big_sl = case_when(
          Sepal.Length > mean(Sepal.Length) + sd(Sepal.Length) ~ TRUE,
          Sepal.Length <= mean(Sepal.Length) + sd(Sepal.Length) ~ FALSE
          ),
         
         big_sw = case_when(
          Sepal.Width > mean(Sepal.Width) + sd(Sepal.Width) ~ TRUE,
          Sepal.Width <= mean(Sepal.Width) + sd(Sepal.Width) ~ FALSE
          ),
         
         big_pl = case_when(
           Petal.Length > mean(Petal.Length) + sd(Petal.Length) ~ TRUE,
           Petal.Length <= mean(Petal.Length) + sd(Petal.Length) ~ FALSE
          ),
         
         big_pw = case_when(
           Petal.Width > mean(Petal.Width) + sd(Petal.Width) ~ TRUE,
           Petal.Width <= mean(Petal.Width) + sd(Petal.Width) ~ FALSE
          )
        ) %>%
  
  rowwise() %>%
  
  mutate(big_cnt = sum(c(big_sl, big_sw, big_pl, big_pw))) %>%
  
  arrange(desc(big_cnt))



###################################################
### TIDYR1
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

colnames(stocks)

# Instal & load package sjmisc to transpose data:
install.packages('sjmisc')
library(sjmisc)

data_stocks <- stocks %>% rotate_df()
colnames(data_stocks)

# Convert first row to column names:
names(data_stocks) <- data_stocks %>% slice(1) %>% unlist()
# Remove 1st row
data_stocks <- data_stocks %>% slice(-1)
data_stocks %>% View()

colnames(data_stocks)

# Check data type in tibble > there are no numeric values
as.tibble(data_stocks)

data_stocks <- data_stocks %>%
  # Convert values in columns to numbers:
  transform(CAC = as.numeric(CAC), 
            DAX = as.numeric(DAX),
            FTSE = as.numeric(FTSE),
            SMI = as.numeric(SMI)) %>%
  # Calculate mean for each column:
  summarise(mean_CAC = round(mean(CAC),2),
            mean_DAX = round(mean(DAX),2),
            mean_FTSE = round(mean(FTSE),2),
            mean_SMI = round(mean(SMI),2))


###################################################
### DPLYR + TIDYR
###################################################

# Using who data set check if % of women with diagnosed tuberculosis
# has changed after year 2000?

?who

library(stringr)

who_clean <- who %>% pivot_longer(starts_with('new'), names_to = 'code', values_to = 'new_cases') %>%
  # add diagnosis and gender column based on code:
  mutate(
    diagnosis = str_extract(code, '(rel|sn|sp|ep)'),
    gender = str_extract(code, '(m|f)'),
    # extract 2 to 4 digits at the end of the code column:
    age = str_extract(code, '\\d{2,4}$'),
    age = case_when(
      age == '014' ~ '0-14',
      age == '1524' ~ '15-24',
      age == '2534' ~ '25-34',
      age == '3544' ~ '35-44',
      age == '4554' ~ '45-54',
      age == '5564' ~ '55-64',
      age == '65' ~ '65+'
      ),
    diagnosis = factor(diagnosis),
    gender = factor(gender),
    # ordered factor:
    age = factor(age, ordered = TRUE, levels = c('0-14','15-24','25-34','35-44','45-54','55-64','65+'))
    ) %>%
  # replace NA values with 0:
  mutate_all(~replace(., is.na(.), 0))

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























