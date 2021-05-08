# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - <put the link here>


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library("dplyr")

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data
ks.projects.201801 <- read.csv("/Users/Arjun/Desktop/Info 201/a4-avaghela-1925502/data:/ks-projects-201801.csv")


# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
col_names <- colnames(ks.projects.201801)

# - How many rows is the data frame?
number_of_rows <- nrow(ks.projects.201801) #There are 378,661 rows in the data frame

# - How many columns are in the data frame?
number_of_cols <- ncol(ks.projects.201801) #There are 15 columns in the data frame


# Use the `summary` function to get some summary information
summary <- summary(ks.projects.201801)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(col_name, data_frame) {
  column <- data_frame[, col_name]
  if (is.numeric(column)) {
    min_ <- min(column)
    max_ <- max(column)
    mean_ <- mean(column)
    return(list(min_, max_, mean_))
  }
  else {
    n_values <- length(unique(column))
    if (length(unique(column)) < 10) {
      unique_values <- unique(column)
      return(list(n_values, unique_values))
    }
    else {
      sample_ <- sample(column, 10)
      return(list(n_values, sample_))
    }
  }
}


# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name

currency_info <- get_col_info("currency", ks.projects.201801)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop

get_summary_info <- function(dataframe) {
  column <- colnames(dataframe)
  sapply(column, get_col_info, dataframe)
}


# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
summary_info <- get_summary_info(ks.projects.201801)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# 1. My first observation was how extreme the pledged amounts minimum and maximum was 
#    (min: 0, max: 20,338,986 , mean: 9682.979) and how much money people allocated to it.
# 2. Of the random country sampling, it seems that the U.S. is the most common in using Kickstarter
# 3. Something that I was wondering was if there is an association between the state that the Kickstarters
#    end up in and the amount of backers that they have


# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
name_projects_highest_goal <- ks.projects.201801[ks.projects.201801["goal"] == max(ks.projects.201801["goal"]), ] %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?
category_projects_lowest_goal <- ks.projects.201801[ks.projects.201801["goal"] == min(ks.projects.201801["goal"]), ] %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
number_projects_deadline_2018 <- ks.projects.201801 %>%
  filter(substr(deadline, 1, 4) == "2018") %>%
  pull(ID) %>%
  length()

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
proportion_projects_not_successful <- 1 - (nrow(ks.projects.201801[ks.projects.201801["state"] == "successful", ]["ID"]) 
                                           / nrow(ks.projects.201801))

# What was the amount pledged for the project with the most backers?
amount_pledged_for_projects_with_most_backers <- ks.projects.201801 %>%
  filter(usd_pledged_real == max(usd_pledged_real, na.tm=T)) %>%
  pull(pledged)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
most_money_pledged_to_failed_project <- ks.projects.201801 %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(name)
most_money_pledged_to_failed_project

# How much total money was pledged to projects that weren't marked successful?
total_money_pledged_to_unseccessful_projects <- ks.projects.201801 %>%
  filter(state != "successful") %>%
  pull(usd_pledged_real) %>%
  sum()

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
category_with_most_money_pledged <- ks.projects.201801 %>%
  group_by(category) %>%
  summarize(usd_pledged_real = sum(usd_pledged_real, na.rm=T)) %>%
  filter(usd_pledged_real == max(usd_pledged_real, na.rm=T)) %>%
  pull(category)
category_with_most_money_pledged

# Which country had the most backers? 
country_with_most_backers <- ks.projects.201801 %>%
  group_by(country) %>%
  summarize(backers = sum(backers, na.rm=T)) %>%
  filter(backers == max(backers, na.rm=T)) %>%
  pull(country)
country_with_most_backers

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
year_with_most_money_pledged <- ks.projects.201801 %>%
  mutate(year = substr(launched, 1, 4)) %>%
  group_by(year) %>%
  summarize(usd_pledged_real = sum(usd_pledged_real, na.rm=T)) %>%
  filter(usd_pledged_real == max(usd_pledged_real, na.rm=T)) %>%
  pull(year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from:
# I chose to use the lauched dates to get the year from rather than the deadline because 
# people may have chosen any date either soon or far for the deadline but the launched date 
# is more representative of when the kickstarted began

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
three_main_categories_in_2018 <- ks.projects.201801 %>%
  mutate(year = substr(launched, 1, 4)) %>%
  filter(year == "2018") %>%
  group_by(category) %>%
  summarize(backers = sum(backers, na.rm=T)) %>%
  top_n(3, backers) %>%
  pull(category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_day_of_week_to_launch_projects <- ks.projects.201801 %>%
  mutate(count = 1) %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarize(count = sum(count, na.rm=T)) %>%
  filter(count == max(count, na.rm=T)) %>%
  pull(day)
  
# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
most_common_day_of_week_to_launch_projects <- ks.projects.201801 %>%
  mutate(count = 1) %>%
  mutate(count_suc = (state == "successful")) %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarize(count = sum(count, na.rm=T), count_suc = sum(count_suc, na.rm=T)) %>%
  mutate(rate = count_suc / count) %>%
  filter(rate == min(rate, na.rm=T)) %>%
  pull(day)