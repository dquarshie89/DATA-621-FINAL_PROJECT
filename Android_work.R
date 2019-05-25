library(tidyverse)
library(tidyr)
library(dplyr)
library(psych)
library(DataExplorer)

#read file


google <- read.csv("https://raw.githubusercontent.com/dquarshie89/DATA-621-FINAL_PROJECT/master/googleplaystore.csv",header = TRUE)

#save column names
data_columns <-colnames(google)

column_desc <- c("Application name","Category the app belongs to",
                 "Overall user rating of the app (as when scraped)","Number of user reviews for the app (as when scraped)",
                 "Size of the app (as when scraped)","Number of user downloads/installs for the app (as when scraped)",
                 "Paid or Free","Price of the app (as when scraped)",
                 "Age group the app is targeted at - Children / Mature 21+ / Adult","An app can belong to multiple genres (apart from its main category)","Date when the app was last updated on Play Store (as when scraped)",
                 "Current version of the app available on Play Store (as when scraped)","Min required Android version (as when scraped)")


col_desc <- data.frame(data_columns, column_desc)

#Data information
colnames(col_desc) <- c("Columns","Description")
print(col_desc)


summary(google)

google_clean <- read.csv("https://raw.githubusercontent.com/dquarshie89/DATA-621-FINAL_PROJECT/master/googleplaystore_clean.csv",header = TRUE)

#Categories of the apps
unique(google_clean$Category)

summary(google_clean)



#select variables

google_clean <- google_clean %>%  select(App, Category, Rating, Reviews, size_adjusted, Installs, Type, Price, Content.Rating, Genres)

google_clean <-google_clean %>% distinct(App,.keep_all = TRUE)


head(google_clean)

#missing data
plot_missing(google_clean)
google_clean <- google_clean %>% filter(!is.na(google_clean$Rating))

#Remove $ sign from price
google_clean$Price = as.numeric(gsub("\\$", "", google_clean$Price))


#above average target value
google_clean$above_avg <- ifelse(google_clean$Rating >= mean(google_clean$Rating, na.rm = TRUE), 1, 0)


describe(google_clean)

#historgram
plot_histogram(google_clean)

google_clean %>% filter(Reviews > 20000000)




describe(google_clean)

#select 
plot_histogram(google_clean)


#Installs
library(ggplot2)
google_clean %>%  group_by(Category) %>%  filter(!is.na(Installs)) %>% summarise(meanInstall = mean(Installs)) %>% 
  ggplot(mapping = aes(x = Category, y = meanInstall)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("Average Number of Downloads by Categories") + ylab("Number of Installs") + 
  guides(fill=FALSE)


#Reviews
google_clean %>%  group_by(Category) %>%  filter(!is.na(Reviews)) %>% summarise(meanReviews = mean(Reviews)) %>% 
  ggplot(mapping = aes(x = Category, y = meanReviews)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("Average Reviews by Categories") + ylab("Average Reviews") + 
  guides(fill=FALSE)

#Rating
google_clean %>%  group_by(Category) %>%  filter(!is.na(Rating)) %>% summarise(meanRating = mean(Rating)) %>% 
  ggplot(mapping = aes(x = Category, y = meanRating)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("Average Rating by Categories") + ylab("Average Rating") + 
  guides(fill=FALSE)


#Count of Above Average
google_clean %>%  group_by(Category) %>%  filter(!is.na(above_avg)) %>% summarise(total_target= sum(above_avg)) %>% 
  ggplot(mapping = aes(x = Category, y = total_target)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("count of Above Average Ratings by Category") + ylab("Average Reviews") + 
  guides(fill=FALSE)


#Log of skewed data points.
google_clean$install_log <- log(google_clean$Installs+1)
google_clean$review_log <- log(google_clean$Reviews+1)

library(ggplot2)
google_clean %>%  group_by(Category) %>%  filter(!is.na(Installs)) %>% summarise(meanInstalls = mean(install_log)) %>% 
  ggplot(mapping = aes(x = Category, y = meanInstalls)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("Average Number of Downloads by Categories") + ylab("Number of Installs") + 
  guides(fill=FALSE)


#Boxplot
plot_boxplot(
  data = google_clean,
  by = "Rating")+ 
  geom_jitter()


#split for Train and Test
set.seed(121)


#Models
split <- sample(1:nrow(google_clean), .8*nrow(google_clean))
experiment <- google_clean[-split,]
train <- google_clean[split,]

#ML

#Poisson
pois <- glm(above_avg ~ install_log + review_log + Type + Category+ Price + Content.Rating, data=train, family="poisson")
summary(pois)

#Poisson backward elimination
pois_red <- glm(above_avg ~ install_log + review_log + Category+ Price , data=train, family="poisson")
summary(pois_red)


#Quasi 
quas <- glm(above_avg ~ install_log + review_log + Type + Category+ Price + Content.Rating, data=train , family="quasi")
summary(quas)

#Quasi backward elimination
quas_red <- glm(above_avg ~ install_log + review_log  + Category+ Price, data=train , family="quasi")
summary(quas_red)

#Binomial
bino <- glm(above_avg ~ install_log + review_log + Type + Category+ Price + Content.Rating, data=train , family="binomial")
summary(bino)

#Binomial backward elimination
bino_red <- glm(above_avg ~ install_log + review_log + Category+ Price, data=train , family="binomial")
summary(bino_red)

exp(bino$coefficients)
confint.default(bino)

binom <- predict(bino,type = 'response' )
summary(binom)

train$bino <- predict(bino, type = 'response')
table(actual = train$above_avg, pred = round(fitted(bino)))




exp(bino_red$coefficients)
confint.default(bino_red)

b <- predict(bino_red,type = 'response' )
summary(b)
train$bino_red <- predict(bino_red, type = 'response')
table(actual = train$above_avg, pred = round(fitted(bino_red)))

#quasi

exp(quas_red$coefficients)
confint.default(quas_red)

quasi_red <- predict(quas_red,type = 'response' )
summary(quasi_red)

train$quas_red <- predict(quas_red, type = 'response')
table(actual = train$above_avg, pred = round(fitted(quas_red)))


#poisson

exp(pois_red$coefficients)
confint.default(pois_red)

poisso_red_red <- predict(pois_red,type = 'response' )
summary(poisso_red_red)

train$pois_red <- predict(pois_red, type = 'response')
table(actual = train$above_avg, pred = round(fitted(pois_red)))


X <- predict(pois_red,type = 'response' )
experiment$pois_red <- predict(pois_red)




