

a = 1
b = 2
c = -1

a
ls ()

# load the package

library(dslabs)
class(murders)
str(murders)
data(murders)
names(murders)
murders$region

codes <- c(italy=380, canada=453, egypt=439)
codes

sort(murders$total)
sort(murders$region)

x <- c(31, 4, 15, 92, 65)
x
sort(x)
order(x)


index <- order(murders$total)
murders$abb[index]

max(murders$total)
which.max(murders$total)

i.max <- which.max(murders$total)
murders$state[i.max]

i.min <- which.min(murders$total)
murders$state[i.min]


index <- match(c("New York", "Florida", "Texas"), murders$state)
index

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)



# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

murders$state[which.max(murders$rate)]

boxplot(rate~region, data = murders)

?log
log4(1024)
log(1024, 4)



# Given coefficients
a <- 2
b <- -1
c <- -4

# Calculate the discriminant
discriminant <- b^2 - 4 * a * c

# Calculate the two solutions
solution1 <- (-b + sqrt(discriminant)) / (2 * a)
solution2 <- (-b - sqrt(discriminant)) / (2 * a)

# Print the solutions
solution1
solution2

library(dslabs)
data(movielens)

class(title)
str(movielens)
class(movielens$title)
class(movielens$genres)
nlevels(movielens$genres)

x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)
min(x)
which.min(x)
max(x)
which.max(x)


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
hour <- time/60
speed <- distance/hour

hour
speed

library(dslabs)
data(olive)
head(olive)
str(olive)

#create a scatter plot
plot(olive$palmitic, olive$palmitoleic,
     xlab = "Palmitic Acid (%)",
     ylab = "Palmitoleic Acid (%)",
     main = "Scatterplot of Palmitic vs. Palmitoleic Acid")
hist(olive$eicosenoic)


boxplot(palmitic ~ region, data = olive,
        main = "Boxplot of Palmitic Acid Percentage by Region",
        xlab = "Region", ylab = "Palmitic Acid (%)")
?boxplot
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

murders <- mutate(murders, rate = total/population * 100000)
filter(murders, rate <= 0.71)

new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)
new_table
?select

murders %>% select(state, region, rate) %>% filter(rate<=0.71)


grades <- data.frame(names=c("Alex", "Anna", "Artemida", "Kos"),
                     exam_1=c(89, 87, 56, 34),
                     exam_2=c(45, 89, 34, 54),
                     stringsAsFactors = FALSE)
grades
class(grades$names)


##############################################################################
#####################2.2 Summarizing with dplyr###############################

library(dslabs)
data(murders)
murders <- mutate(murders, rate = total/population * 10^5)

murders

## minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarise(minimum = min(rate),
            median = median(rate),
            maximum = max(rate))
s

# accessing the components with the accessor $
s$median
s$minimum

# average rate unadjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)

us_murder_rate
class(us_murder_rate)

# the pull function can return it as a numeric value
us_murder_rate %>% pull(rate)

# using pull to save the number directly
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate

# us_murder_rate is now stored as a number
class(us_murder_rate)

# group by region
murders %>% group_by(region)

# summarize after grouping
murders %>% 
  group_by(region) %>%
  summarize(median = median(rate))

# order the states by population size
murders %>% arrange(population) %>% head()

# order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states by murder rate
murders %>% top_n(10, rate) 

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)


###############################################################################
#########################Section 2: Data Wrangling ############################
########################## 2.3 data.table #####################################

# install the data.table package before you use it!
install.packages("data.table")
library(dslabs)
data(murders)

# load data.table package
library(data.table)

# convert the data frame into a data.table object
murders <- setDT(murders)

################################################################################
##################### Selecting in dplyr vs. data.table ########################

# selecting in dplyr
select(murders, state, region)

# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[,.(state, region)] |> head()

################################################################################
############ adding or changing a column in dplyr vs. data.table ###############

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)

## To add multiple column
murders[,  ":=" (rate = total / population * 100000, rank = rank(population))]
head(murders)

################################################################################
####### subsetting & combining filter and select in dplyr vs. data.table #######

# subsetting in dplyr
filter(murders, rate <= 0.7)

# subsetting in data.table
murders[rate <= 0.7]

# combining filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state, rate)

# combining filter and select in data.table
murders[rate <=0.7, .(rate, state)]

################################################################################
#################### Summarizing with dplyr vs. data.table #####################

data(heights)
heights <- setDT(heights)

# summarizing in dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))

# summarizing in data.table
s <- heights[, .(average = mean(height), standard_deviation = sd(height))]

# subsetting and then summarizing in dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# subsetting and then summarizing in data.table
s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]

# previously defined function
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]

################################################################################
########################## Sorting data frame ##################################

# order by population
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] |> head()

# order by region and then murder rate
murders[order(region, rate)] |> head()


################################################################################
############################### Tibbles ########################################

# A tbl (pronounced "tibble") is a special kind of data frame.
# Tibbles are the default data frame in the tidyverse.
# Tibbles display better than regular data frames.
# Subsets of tibbles are tibbles, which is useful because tidyverse functions require data frames as inputs.
# Tibbles will warn you if you try to access a column that doesn't exist.
# Entries in tibbles can be complex - they can be lists or functions.
# The function group_by() returns a grouped tibble, which is a special kind of tibble.

# view the dataset
murders %>% group_by(region)

# see the class
murders %>% group_by(region) %>% class()

# compare the print output of a regular data frame to a tibble
gapminder
as_tibble(gapminder)

# compare subsetting a regular data frame and a tibble
class(murders[,1])
class(as_tibble(murders)[,1])

# access a column vector not as a tibble using $
class(as_tibble(murders)$state)

# compare what happens when accessing a column that doesn't exist in a regular data frame to in a tibble
murders$State
as_tibble(murders)$State

# create a tibble
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

################################# QUIZ #########################################

library(dslabs)
data(heights)
options(digits = 3)   

# Q-1 First, determine the average height in this dataset.  
# Then create a logical vector ind with the indices for those individuals who are above average height.
# How many individuals in the dataset are above average height?

avg_height <- mean(heights$height)
print(avg_height)

ind <- heights$height > avg_height    
sum(ind)
# or 
num_above_avg <- sum(ind)
num_above_avg

# Q-2 How many individuals in the dataset are above average height and are female?
ind_female_above_avg <- heights$height > avg_height & heights$sex == "Female"

num_female_above_avg <- sum(ind_female_above_avg)
num_female_above_avg

# Q-3 What proportion of individuals in the dataset are female?
only_female <- sum(heights$sex == "Female")
total_individuals <- nrow(heights)
proportion_females <- only_female / total_individuals
proportion_females

# Q-4a Determine the minimum height in the heights dataset.
min_height <- min(heights$height)
print(min_height)

# Q-4b Use the match() function to determine the index of the first individual with the minimum height.
index_min_height <- match(min_height, heights$height)
index_min_height 

# Q-4c Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
individual_sex <- heights$sex[index_min_height]
individual_sex
# OR
heights$sex[index_min_height]

# Q-5a Determine the maximum height.
max_height <- max(heights$height)
max_height

# Q-5b Write code to create a vector x that includes the integers between 
# the minimum and maximum heights in this dataset (as numbers).
x <- 50:82

# Q-5c How many of the integers in x are NOT heights in the dataset?
# Use the sum() and %in% functions in addition to the ! operator.
x <- 50:82
not_in_dataset <- !x %in% heights$height
num_not_in_dataset <- sum(not_in_dataset)
num_not_in_dataset

# Q-6a Using the heights dataset, create a new column of heights in centimeters named ht_cm.
#Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2.

# What is the height in centimeters of the 18th individual (index 18)?
heights2 <- mutate(heights, ht_cm = height * 2.54)
head(heights2)
heights2$ht_cm[18]

# Q-6b What is the mean height in centimeters?
mean(heights2$ht_cm)

# Q -7a Create a data frame females by filtering the heights2 data to contain only female individuals
# How many females are in the heights2 dataset?

females <- heights2 %>% filter(sex == "Female")
sum(heights2$sex == "Female")

# Q-7b What is the mean height of the females in centimeters?
females <- heights2 %>% filter(sex == "Female")

mean_height_females_cm <- mean(females$ht_cm)
mean_height_females_cm

################################################################################
######################## 3.1 conditionals and functions ########################

# an example showing the general structure of an if-else statement
a <- 0
if (a != 0) {
  print(1/a)
} else {
  print("No reciprocal for 0.")
}

# an example showing the general structure of an if-else statement with a <- 2
a <- 2
if (a != 0) {
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5

library(dslabs)
data("murders")

murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if (murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}

# changing the condition to < 0.25 changes the result

if (murder_rate[ind] < 0.25){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}

# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
result

a <- c(3,1,6,8.76,5)
ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
sum(is.na(na_example))

#replacing missing values
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))
no_nas

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

z <- c(TRUE, TRUE,TRUE, TRUE)
all(z)

# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

x <- 1:100
avg(x)

# we see that the above function and the pre-built R mean() function are identical
identical(mean(x), avg(x))

################################################################################
############################### For loops ######################################

# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

compute_s_n(3)
compute_s_n(100)

# a for-loop for our summation
m <- 25

# create an empty vector
s_n <- vector(length = m)
for (n in 1:m) {
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)

################################################################################
################################ Quiz - 3 ######################################

library(dslabs)
data("heights")

#Q-1 Write an ifelse() statement that returns 1 if the sex is Female and 2 if the sex is Male.
# What is the sum of the resulting vector?

result_vector <- ifelse(heights$sex == "Female", 1, 2)

sum_vector <- sum(result_vector)
sum_vector
?heights

# Q-2 Write an ifelse() statement that takes the height column and returns 
# the height if it is greater than 72 inches and returns 0 otherwise.

# What is the mean of the resulting vector?

res_vec <- ifelse(heights$height > 72, heights$height, 0)
mean_vector <- mean(res_vec)
mean_vector

# Q-3 Write a function inches_to_ft that takes a number of inches x and returns 
# the number of feet. One foot equals 12 inches.

# Q-3A What is inches_to_ft(144)?

inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)

# Q-3B How many individuals in the heights dataset have a height less than 5 feet?

# Filter individuals with height less than 60 inches
individuals_less_than_5ft <- sum(heights$height < 60)
individuals_less_than_5ft

test <- vector(length = 5)
for (i in 1:5){
  test[i] <- i^2
}
test
i


################################################################################
######################## How to import Data in R ###############################

# Copy the spreadsheet containing the US murders data (included as part of the dslabs package) 
filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")

# Once the file is copied, import the data with a line of code. Use the read_csv function from the readr package (included in the tidyverse)
library(tidyverse)
dat <- read_csv(filename)

# Load it directly
library(readr)

# Open the file to take a look or use the function read_lines to look at a few lines
read_lines("murders.csv", n_max = 3)

# Read-in the data into R from the .csv suffix 
dat <- read_csv(filename)

# Confirm that the data has in fact been read-in 
View(dat)

# Use the full path for the file
dat <- read_csv(fullpath)

# Load the readxl package using
library(readxl)

# Our dslabs package is on GitHub and the file we downloaded with the package has a url
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

# Use read_csv file to read these files directly
dat <- read_csv(url)

