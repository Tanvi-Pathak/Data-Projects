# reading the CSV file:
nyc_data_final <- read.csv("C:/Users/tanvi/OneDrive/Desktop/R files/nyc_data_final.csv")

# Loading packages-
library(tidyverse)
library(ggplot2)
library(gridExtra)

#joining the two datasets- the crime and restaurant data with the airbnb data:
nyc_data_resturants <- read.csv("C:/Users/tanvi/Downloads/nyc_data_resturants.csv")

nyc_data_final <- nyc_data_final |>
  left_join(nyc_data_resturants, by = c("id" = "id"))

#Scatterplot of all the variables in the dataset:
pairs(nyc_data_final[, c(7,8,10,11,12, 14, 15, 16, 17)])

#lets take a closer look at some of the interesting plots:
##price VS floor:
plot(nyc_data_final$price, nyc_data_final$floor,
     main = "Price VS Floor",
     xlab = "Price", ylab = "Floor")
##price VS noise:
plot(nyc_data_final$price, nyc_data_final$noise.dB.,
     main = "Price VS Noise",
     xlab = "Price", ylab = "Noise")
##price VS restaurants:
plot(nyc_data_final$price, nyc_data_final$resturants,
     main = "Price VS Restaurants",
     xlab = "Price", ylab = "Restaurants")
##restaurants VS floor:
plot(nyc_data_final$resturants, nyc_data_final$floor,
     main = "Restaurants VS Floor",
     xlab = "Restaurants", ylab = "Floor")
##restaurants VS noise:
plot(nyc_data_final$resturants, nyc_data_final$noise.dB.,
     main = "Restaurants VS Noise",
     xlab = "Restaurants", ylab = "Noise")
##restaurants VS latitude:
plot(nyc_data_final$latitude, nyc_data_final$resturants,
     main = "Latitude VS Restaurants",
     xlab = "Latitude", ylab = "Restaurants")

#boxplots of categorical variables:
##neighborhood group with restaurants:
restaurants <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
                      y = resturants)) +
  geom_boxplot() +
  ggtitle("Borough VS Number of Restaurants")

##neighbourhood group with noise:
noise <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = noise.dB.)) +
  geom_boxplot() +
  ggtitle("Borough VS Amount of Noise")

##neighbourhood group with number of reviews:
reviews <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = number_of_reviews)) +
  geom_boxplot() +
  ggtitle("Borough VS Number of Reviews")

##neighborhood group versus price:
price <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = price)) +
  geom_boxplot() +
  ggtitle("Borough VS Price")

##neighbourhood group and floor:
floor <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = floor)) +
  geom_boxplot() +
  ggtitle("Borough VS Floor")
##neighbourhood group and reviews_per month:
monthly_reviews <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = reviews_per_month)) +
  geom_boxplot() +
  ggtitle("Borough VS Monthly Reviews")
#neighbourhood group and number of nights:
nights <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = minimum_nights)) +
  geom_boxplot() +
  ggtitle("Borough VS Nights")
#neighbourhood group and latitude
latitude <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = latitude)) +
  geom_boxplot() +
  ggtitle("Borough VS latitude")
#neighbourhood group and longitude
longitude <- nyc_data_final |>
  ggplot(aes(x = neighbourhood_group, 
             y = longitude)) +
  geom_boxplot() +
  ggtitle("Borough VS longitude")

## get all the boxplots on one page:
grid.arrange(restaurants, noise, reviews, price, floor, monthly_reviews, nights, latitude, longitude, ncol = 3)

##type of room with restaurants:
restaurants1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = resturants)) +
  geom_boxplot() +
  ggtitle("Room Type VS Number of Restaurants")

##type of room group with noise:
noise1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = noise.dB.)) +
  geom_boxplot() +
  ggtitle("Room Type VS Amount of Noise")

##type of room with number of reviews:
reviews1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = number_of_reviews)) +
  geom_boxplot() +
  ggtitle("Room Type VS Number of Reviews")

##type of room versus price:
price1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = price)) +
  geom_boxplot() +
  ggtitle("Room Type VS Price")

##tyoe of room and floor:
floor1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = floor)) +
  geom_boxplot() +
  ggtitle("Room Type VS Floor")
##type of room and reviews_per month:
monthly_reviews1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = reviews_per_month)) +
  geom_boxplot() +
  ggtitle("Room Type VS Monthly Reviews")
#type of room nights:
nights1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = minimum_nights)) +
  geom_boxplot() +
  ggtitle("Room Type VS Nights")
#type of room and latitude
latitude1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = latitude)) +
  geom_boxplot() +
  ggtitle("Room Type VS latitude")
#type of room and longitude
longitude1 <- nyc_data_final |>
  ggplot(aes(x = room_type, 
             y = longitude)) +
  geom_boxplot() +
  ggtitle("Room Type VS longitude")

##get all the boxplots on one page:
grid.arrange(restaurants1, noise1, reviews1, price1, floor1, monthly_reviews1, nights1, latitude1, longitude1, ncol = 3)

#Plotting all the categorical variables:
par(mfrow=c(2,3))#gets all the plots on one page
#let's get a mosaic plot of the room type versus the neighbourhood group- we can see what room type is more common in which enighbourhhod and which is the most common neighbourhood
table(nyc_data_final$neighbourhood_group, nyc_data_final$room_type) |>
  mosaicplot(xlab= "borough", ylab='room type',main='borough VS room type', col=c('pink', 'purple', 'lightblue', 'lightyellow', 'orange'))

#lets take a look at the number of airbnbs per borough.
barplot(table(nyc_data_final$neighbourhood_group),
           width = c(0.30, 0.30, 0.30, 0.30 ,0.30),
        xlab = "Borough",
        ylab = "Count",
        main = "Number of airbnbs per borough",
        col = "lightyellow")
#let's take a look at prices per borough:
mean_price_bourough <- aggregate(nyc_data_final$price, list(nyc_data_final$neighbourhood_group), FUN=mean)
barplot(height = mean_price_bourough$x, name = mean_price_bourough$Group.1,
        width = c(0.30, 0.30, 0.30, 0.30 ,0.30),
        ylim = c(0, 300),
        xlab = "Borough",
        ylab = "Mean Price",
        main = "Average Price of Airbnbs per Borough per borough",
        col = "lightblue")
#lets takea  look at the number restaurants per neighbourhood:
mean_resutaurants_bourough <- aggregate(nyc_data_final$resturants, list(nyc_data_final$neighbourhood_group), FUN=mean)
barplot(height = mean_resutaurants_bourough$x, name = mean_resutaurants_bourough$Group.1,
        width = c(0.30, 0.30, 0.30, 0.30 ,0.30),
        ylim = c(0, 300),
        xlab = "Borough",
        ylab = "Mean Number of Restaurants",
        main = "Average Number of Restaurants per borough",
        col = "pink")
#What is the Most popular neighbourhood in manhattan : which neighbourhood has the most airbnb listings.
manhattan <- subset(nyc_data_final, neighbourhood_group == "Manhattan")
barplot(table(manhattan$neighbourhood),
        width = c(0.30, 0.30, 0.30, 0.30 ,0.30),
        xlab = "Neighbourhood",
        main = "Number of airbnbs per neighbourhood in manhattan",
        col = "purple",
        horiz = TRUE, 
        las = 2)

#Let's now calculate some interesting Outliers based on some of our previous graphs:
#let's look at all the boxplots first so we can visualize the outliers. 
par(mfrow = c(3,3))
boxplot(nyc_data_final$longitude,
        main = "longitude")
boxplot(nyc_data_final$latitude,
        main = "latitude")
boxplot(nyc_data_final$price,
        main = "price")
boxplot(nyc_data_final$minimum_nights,
        main = "nights")
boxplot(nyc_data_final$number_of_reviews,
        main = "total reviews")
boxplot(nyc_data_final$reviews_per_month,
        main = "reviews per month")
boxplot(nyc_data_final$floor,
        main = "floor")
boxplot(nyc_data_final$noise,
        main = "noise")
boxplot(nyc_data_final$resturants[is.finite(nyc_data_final$resturants)],
        main = "restaurants")


##based on all the plots we visualized, we can be sure that there are definitely outliers in the data, let's take a look.
##all the numerical values:
###price:
Q1 <- 69.0
Q3 <- 175.0
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
range(nyc_data_final$price[nyc_data_final$price > upper_bound]) # the range of outliers is 335 10000

###longitude:
summary(nyc_data_final$longitude)
Q1 <- -73.98
Q3 <- -73.94
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
range(nyc_data_final$longitude[nyc_data_final$longitude < lower_bound | nyc_data_final$longitude > 0])
#the most noticeable outlier is 73.86960(these values should be negative)

###nights:
summary(nyc_data_final$minimum_nights)
Q1 <- 1
Q3 <- 5
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
range(nyc_data_final$minimum_nights[nyc_data_final$minimum_nights > upper_bound])
#range of outliers is 12 to 1250(most noticeable outlier is 1250 nights)

##floor:
summary(nyc_data_final$floor)
Q1 <- 1
Q3 <- 1
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
range(nyc_data_final$floor[nyc_data_final$floor > upper_bound])
#most noticeable outlier is the 72nd floor

###noise:
summary(nyc_data_final$noise.dB.)
Q1 <- 56.05
Q3 <- 69.06
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
range(nyc_data_final$noise.dB.[nyc_data_final$noise.dB.< lower_bound | nyc_data_final$noise.dB. > upper_bound])
#most noticeable outlier is 98 db of noise 

#Hypthesis Testing:
#Restaurants Brooklyn VS Manhattan:
Manhattan <- subset(nyc_data_final, neighbourhood_group == "Manhattan")
Brooklyn <- subset(nyc_data_final, neighbourhood_group == "Brooklyn")
Hells <- subset(Manhattan, neighbourhood == "Hell's Kitchen")
Harlem <- subset(Manhattan, neighbourhood == "Harlem")

#getting standard deviations of x and y:
sd(Manhattan$resturants)
sd(Brooklyn$resturants)

z.test(Manhattan$resturants, Brooklyn$resturants, alternative = "greater", sigma.x = 168.8835, sigma.y = 135.1946)

#price of Harlem VS hells Kitchen:
sd(Harlem$price)
sd(Hells$price)

z.test(Hells$price, Harlem$price, alternative = "greater", sigma.x = 190.3412, sigma.y = 166.8375)

#noise:
sd(Harlem$noise.dB.)
sd(Hells$noise.dB.)

z.test(Hells$noise.dB., Harlem$noise.dB., alternative = "greater", sigma.x = 0.5595364, sigma.y = 0.4520683)

#Number of reviews per month versus neighbourhood:
#Taking a look at neighbourhoods within Manhattan:
#in order to calculate the standard devitiations of x and y I remove the NA values because there are too many. 
sd(Harlem$reviews_per_month, na.rm = TRUE)
sd(Hells$reviews_per_month, na.rm = TRUE)
z.test(Hells$reviews_per_month, Harlem$reviews_per_month, alternative = "greater", sigma.x = 1.977636, sigma.y = 1.495647)

