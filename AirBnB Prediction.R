#load packages
library(tidyverse)
library(ggplot2)
library(gridExtra)

#initial interaction between variables:
#based on our EDA we can take a look at our initial idea- 
#I thought that room type, borough type, and number of restaurants played the biggest role in determining airbnb prices. 
#we noticed that the most popular places(that is the area with the most restaurants, had the priciest airbnbs.)
#interestingly, there were also differences within boroughs. So certain places were more popular than others within the borough. 
#We did notice that the most popular borough was Manhattan. 
#so maybe one of the predictors is dependent on the borough predictor. 
#let's try a whole bunch of different models based on our analysis.
#initial model: 
#all the predictors against price. 
#in order to get a decent model I decided to convert the categorical variables into factors since they are currently listed as characters. 
initial_model <- lm(price ~ as.factor(neighbourhood_group) + as.factor(neighbourhood) + as.factor(room_type) + minimum_nights + reviews_per_month + floor + noise.dB. + resturants, data = nyc_data_final)
summary(initial_model)
#take a look at residuals:
res_inititial <- resid(initial_model)
qqnorm(res_inititial)
qqline(res_inititial) 
#based on the summary and residual plot its pretty obvious that this isnt the best model

#let's look at another model and only take a look at the most important variables-
#variables that i thought had the most influence on airbnb price were noise levels, number of restaurants, room type, and neighborhood type
model_1 <- lm(price ~ as.factor(neighbourhood_group) + as.factor(room_type) + noise.dB. + resturants, data = nyc_data_final)
summary(model_1)
#take a look at residuals:
res1 <- resid(model_1)
qqnorm(res1)
qqline(res1) 
ggplot(data=nyc_data_final, aes(res1)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

#lets look at some more models
#while the previous two models were not bad we might be able to come up with better models if we tweak our original model a bit more:

#model 2- neighbourhood, neighbourhood group, number of restaurants, noise levels, and room type:
model_2 <- lm(price ~ as.factor(neighbourhood_group) + as.factor(neighbourhood) + as.factor(room_type) + noise.dB. + resturants, data = nyc_data_final)
summary(model_2)
#honestly, this model isn't too different from the previous model
#let's try using an interaction term.
#before we do that it is important to note that we can either drop the neighbourhood variable or try to reduce it down to a couple neighbourhoods.
#there are way too many categories in the neighbourhood category that it can lead to a number of problems with our data
#lets try the same model without the neighbourhood cateogry and then we can try the same model by narrowing it down some how.
model_3 <- lm(price ~ as.factor(neighbourhood_group)  + as.factor(room_type) + noise.dB. + resturants, data = nyc_data_final)
summary(model_3)
res3 <- resid(model_3)
qqnorm(res3)
qqline(res3) 
ggplot(data=nyc_data_final, aes(res3)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

#another interesting model would be too see how prices differ within different neighbourhoods so we can take a look at noise * neighbourhood_group
#this will help us capture how noise levels effect property prices differently in various boroughs
model_4 <- lm(price ~ as.factor(neighbourhood_group)  * noise.dB. + as.factor(room_type) + resturants, data = nyc_data_final)
summary(model_4)
#this model has given us the best R^2 so far at 0.9517.
res4 <- resid(model_4)
qqnorm(res4)
qqline(res4) 
#this looks pretty decent - most residuals seem to be normally fitted around 0 except for the obvious couple outliers that skew the plot
#now that we know that the interaction term is doing pretty well, we can try more combinations
model_5 <- lm(price ~ as.factor(neighbourhood_group)  * noise.dB. + as.factor(neighbourhood_group) * as.factor(room_type) + as.factor(neighbourhood_group) * resturants, data = nyc_data_final)
summary(model_5)
#model 6:
model_6 <- lm(price ~ as.factor(neighbourhood_group)  * noise.dB. + as.factor(neighbourhood_group) * resturants, data = nyc_data_final)
summary(model_6)
#so I tried without the room type as a predictor but my R^2 seems to be higher when I use room type as a predictor. 
#lets try running the model (model 5) on our test dataset and calculating the MSE. 

#loading the test data:
test_data <- read.csv("C:/Users/tanvi/OneDrive/Desktop/R files/test_data.csv")
resturants_test <- read.csv("C:/Users/tanvi/Downloads/resturants_test (1).csv")

#adding restaurant data:

test_data <- test_data |>
  left_join(resturants_test, by = c("id" = "id"))

#testing model 5:
predicted_prices <- data.frame(ID = test_data$id, price = predict(model_5, test_data))

#calculating MSE:
mean(model_5$residuals^2)

#MSE is 2273.886. Its decent but not the best so lets test some of our other models and see if we can make this number any smaller. 
#if I can't I can try to make another model of best fit maybe one with a higher R^2 value.

#testing model 6:
prediction_dataset <- data.frame(pred = predict(model_6), actual = nyc_data_final$price)
#calculating MSE:
mean(model_6$residuals^2)

mean((prediction_dataset$actual - prediction_dataset$pred)^2)
#so this model has a worse MSE. This means that the lower the R^2 value the worse the MSE score. 
#so in order to get a better MSE value I need to create a model with a really high r squared value. 

#the best mode so far is model 5. 
summary(model_5)
#R squared for this is 0.9616
#I want to see if I can get a better value than this.
#let's see.
#this model uses 3 interaction terms and uses room type, number of restaurants, noise, and neighbourhood group.

#I want to take a look at the most meaningful relationships:
#let's look at a more simplified model:
simplified_model <- lm(price ~ as.factor(neighbourhood_group) * noise.dB. + as.factor(room_type) * resturants, data = nyc_data_final)
summary(simplified_model)
#not quite good

#lets try a more meaningful interaction:
model_7 <- lm(price ~ as.factor(room_type) *  as.factor(neighbourhood_group) + noise.dB. * resturants , data = nyc_data_final)
summary(model_7)
#testing model 7:
prediction_dataset <- data.frame(id = test_data$id, pred = predict(model_7, test_data))
#calculating MSE:
mean(model_7$residuals^2)

#so it seems like the best MSE score is when I use model 5. 
#this model definitely has some issues that are addressed in the PPT.

#One issue is that there is possible multicollinearity. 
#this can be addressed by removing certain variable or doing some sort of PCA or factor analysis. 

#saving the model as a csv:
write.csv(predicted_prices, "predicted_prices.csv", row.names=TRUE)


