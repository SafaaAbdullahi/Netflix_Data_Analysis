# Netflix Data Cleaning and Preparation

install.packages("readxl")     # to read Excel
install.packages("dplyr")      # data manipulation
install.packages("ggplot2")    # visualization
install.packages("janitor")    # clean column names
install.packages("tidyr")      # reshaping
install.packages("lubridate")  # working with dates

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(lubridate)

# ============================
# import the database

dataset <- read_excel("C:\\Users\\hp\\Desktop\\netflix_titles.xlsx")
netflix <- dataset

# ============================
# inspect the dataset 

head(netflix)        
View(netflix)       
glimpse(netflix)    
summary(netflix)  
colnames(netflix)   

# ==========================
# Check for missing values
# ==========================
colSums(is.na(netflix))   # count missing values per column

# ==========================
# Handle missing values
# ==========================
# Drop rows where 'title' is missing (essential info)
netflix <- netflix %>% drop_na(title)
# =================================
# Replace missing ratings with "Unknown"
netflix <- netflix %>% 
  mutate(rating = ifelse(is.na(rating), "Unknown", rating))

# =======================================
netflix <- netflix %>%
  mutate(
    director = ifelse(is.na(director), "Unknown", director),
    cast     = ifelse(is.na(cast), "Unknown", cast),
    country  = ifelse(is.na(country), "Unknown", country),
    duration = ifelse(is.na(duration), "Unknown", duration)
  ) %>%
  mutate(
    director = as.factor(director),
    cast     = as.factor(cast),
    country  = as.factor(country),
    duration = as.factor(duration)
  )

# ======================
# remove duplicates
netflix <- netflix %>% distinct()

str(netflix)
colSums(is.na(netflix))
head(netflix)

#===========================

# Number of rows & columns
dim(netflix)

# Movies vs TV Shows
table(netflix$type)

# Top 10 countries with most titles
netflix %>% group_by(country) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(10)

# Most common ratings
netflix %>% group_by(rating) %>% summarise(count = n()) %>% arrange(desc(count))

# ==========================
# Visualizations
# ==========================
# Movies vs TV Shows
ggplot(netflix, aes(x = type, fill = type)) +
  geom_bar() +
  labs(title = "Movies vs TV Shows on Netflix", x = "Type", y = "Count")

# Titles added per year
netflix %>%
  mutate(year_added = year(date_added)) %>%
  group_by(year_added) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = year_added, y = count)) +
  geom_line(color = "blue") +
  labs(title = "Netflix Titles Added Over Time", x = "Year Added", y = "Number of Titles")

# Top 10 countries
netflix %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(country, -count), y = count, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Countries with Most Netflix Titles", x = "Country", y = "Number of Titles")

# Distribution of content ratings
ggplot(netflix, aes(x = rating, fill = rating)) +
  geom_bar() +
  labs(title = "Distribution of Content Ratings on Netflix", x = "Rating", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==========================
# Save cleaned dataset
# ==========================
write.csv(netflix, "C:/Users/hp/Desktop/netflix_cleaned.csv", row.names = FALSE)























