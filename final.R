library('ggplot2')
library('MASS')
library('dplyr')
library('psych')
library('DataExplorer')
library('mice')
library('car')
library('caret')
library('faraway')
library('stargazer')
library('pROC')
library('stringr')

#Load
apps_url <- 'https://raw.githubusercontent.com/dquarshie89/Data-621/master/AppleStore.csv'
apps <- read.csv(apps_url, header=TRUE)

#Make Ratings a Factor
apps_final$rating_count_tot <- as.factor(apps_final$rating_count_tot)

#Make Is_Game and Target
apps$isgame <- ifelse(apps$prime_genre == 'Games', 1, 0)
apps$over_med <- ifelse(apps$rating_count_tot >= median(apps$rating_count_tot), 1, 0)

#Select Final Fields
apps_final <- apps %>% 
  select(size_bytes, price, rating_count_tot, rating_count_ver,	user_rating, user_rating_ver,cont_rating,
         sup_devices.num,	ipadSc_urls.num,	lang.num, isgame, over_med )

#Make Final Edits
apps_final$cont_rating <- unname(sapply(apps_final$cont_rating, str_replace_all, '[+]', ''))
apps_final$cont_rating <- as.numeric(apps_final$cont_rating)




#write.csv(apps_final, 'apps_final.csv', row.names = FALSE)
#Stats
#Number over and under median
summary(apps_final$over_med==0)

str(apps_final)

#Summary
summary <- describe(apps_final[,c(1:12)])[,c(2:5,8,9,11,12)]
knitr::kable(summary)

#Histogram
plot_histogram(apps_final)
out <- split_columns(apps_final)
plot_histogram(out$continuous)

#Boxplot
plot_boxplot(
  data = apps_final,
  by = "user_rating")+ 
  geom_jitter()

#Number missing 
plot_missing(apps_final)

#split for Train and Test
set.seed(121)
split <- sample(1:nrow(apps_final), .5*nrow(apps_final))
app_train <- apps_final[-split,]
app_test <- apps_final[split,]

#Models
split2 <- sample(1:nrow(app_train), .8*nrow(app_train))
holdout <- app_train[-split2,]
train <- app_train[split2,]


#Poisson
pois <- glm(rating_count_tot ~ . , data=train, family="poisson")
summary(pois)

#Quasi 
quas <- glm(rating_count_tot ~ . , data=train, family="quasi")
summary(quas)

#Quasi Reduced
quas_red <- glm(rating_count_tot ~  rating_count_ver + lang.num + over_med, data=train, family="quasi")
summary(quas_red)



#Results

#Run with Poisson Reduced
finalpreds <- predict(pois, app_test)
finaldf <- cbind(TARGET_FLAG=exp(finalpreds))
summary(finaldf)
summary(app_train)
hist(apps_final$rating_count_tot)


write.csv(finaldf, 'final.csv', row.names = FALSE)

