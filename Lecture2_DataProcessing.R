setwd('/Users/amy/DA101/')
list.files(getwd())

# source("initializer.R")

library(data.table)
library(dplyr)
library(ggplot2)
library(gsheet)
library(tidyverse)
library(tidyr)

# Read data from Gspreadsheet link

url = 'docs.google.com/spreadsheets/d/1xrjENV8X-KBFmW96LZ3-JXYp8UJMnqwp8FGJHGQNXrs/edit#gid=0'
data = data.table(gsheet2tbl(url))

head(data)
str(data)
names(data)
summary(data$age)

# Creating word cloud from column `occupation`

wc_dat = data[, .N, by = 'occupation'][order(-N)]
wc_dat
# Check frequency of occupation

library(wordcloud) # Install neccessary packages (if haven't done so)
library(tm)

wordcloud(words = wc_dat$occupation, freq= wc_dat$N,scale=c(4,0.5),min.freq=1,max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors="black",ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

# Creating pie chart from column `marital_status`

pie(table(data$marital_status))

pie_data = data[, .(pct=round(.N/nrow(data)*100)), by='marital_status']
pie_data = pie_data[, label := paste(pie_data$marital_status, paste(pie_data$pct, '%'))]
pie(pie_data$pct, labels = pie_data$label)

# Creating a bar chart of frequency of each value in column `education`

barplot(sort(table(data$education)), xlab='count', ylab='education', main='Education Level', col = '#008080') #Simple quick solution

edu_freq = data[, .(count = .N), by = 'education'][order(count)] # Creating the data table to plot
edu_freq

ggplot(edu_freq, aes(x = reorder(education, count), y = count)) + # Reorder the categorical variable based on the frequency 
  geom_bar(fill = "#008080", stat='identity') + # More colors here: https://www.rapidtables.com/web/color/RGB_Color.html
  geom_text(aes(label = count), vjust = -0.3) +
  xlab('education') +
  ggtitle('Education Leve') +
  theme(plot.title = element_text(hjust = 0.5))

# Creating a histogram from column `hour_per_week`

hist(data$hour_per_week, breaks = 20, main = 'Histogram of Hour Per Week column', xlab = 'hour_per_week', ylab = 'frequency', col = '#008080')

ggplot(data, aes(x = hour_per_week)) +
  geom_histogram(bins = 30, fill='#008080') +
  ggtitle('Histogram of hour_per_week') +
  theme(plot.title = element_text(hjust = 0.5))

# Creating box plot: education_num by ethnicity

boxplot(education_num~ethnicity,data=data, main="Number of Education Year Data", 
        xlab="ethnicity", ylab="education_num")


ggplot(data, aes(x = ethnicity, y = education_num)) +
  geom_boxplot(width = 0.4, fill = "white")


# Calculate min, max, sd: age

min(data$age)
max(data$age)
sd(data$age)

# Impute missing value using kNN for column native_country

summary(data) # Check missing value in column native_country
data[, native_country := as.factor(native_country)]
data[, .N, by = 'native_country']

library(VIM)
data_1 = kNN(data, variable = 'native_country', k = 5, dist_var = 'ethnicity', makeNA = '?')%>%data.table()

summary(data)
summary(data_1)

data_1[native_country_imp == TRUE, .(native_country)] # Check the imputed values

# Standardize data (i.e. normalize so that we have a mean of 0 and SD of 1)

data[, age_standardized := scale(age)]
mean(data$age)
mean(data$age_standardized)
sd(data$age)
sd(data$age_standardized)

# Normalize data (value range between 0 and 1)

data[, age_normalized := (age-min(age))/(max(age)-min(age))]
mean(data$age)
mean(data$age_normalized)
sd(data$age)
sd(data$age_normalized)
min(data$age_normalized)
max(data$age_normalized)

ggplot(data, aes(x = ethnicity, y = age)) +
  geom_boxplot(width = 0.4, fill = "white")

ggplot(data, aes(x = ethnicity, y = age_normalized)) +
  geom_boxplot(width = 0.4, fill = "white")

ggplot(data, aes(x = ethnicity, y = age_standardized)) +
  geom_boxplot(width = 0.4, fill = "white")

# Reference for pollting:
https://www.statmethods.net/graphs/pie.html
http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/#histogram-plots
