rm(list=ls())
library(MASS)
library(lubridate)
library(ggplot2)
library(dplyr)

work_dir <- "D:/School Stuff/Math/Stats 141A/Data Files/HW 2"
setwd(work_dir)
housing = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/HW 2/housing.rds")

##############
#1

str(housing)
housing$county = sapply(housing$county, as.factor)
housing$price = sapply(housing$price, as.integer)
housing$lsqft = sapply(housing$lsqft, as.integer)
housing$year = sapply(housing$year, as.integer)
str(housing)

summary(housing$county)
housing$county = gsub("county", "County", housing$county, ignore.case = FALSE)
housing$county = gsub("Franciscoe", "Francisco", housing$county, ignore.case = FALSE)
housing$county = gsub("Alpine County", "San Francisco County", housing$county, ignore.case = FALSE)
housing$county = sapply(housing$county, as.factor)
summary(housing$county)

#############
#2

sorted_years = sort(housing$year, decreasing = TRUE)
head(sorted_years, 20)
tail(sorted_years, 20)

which.min(sapply(housing$date, as.Date))
which.max(sapply(housing$date, as.Date))
housing$date[138]
housing$date[44]

#############
#3

housing$monthyear = format(as.Date(housing$date), "%Y-%m")
date_table = table(housing$monthyear)
plot(date_table, main = "Sales during Year-Month", xlab = "Year-Month", ylab = "Sales Count")

avg_price_monthyear = aggregate(price~monthyear, housing, mean, na.rm = TRUE)
ggplot(avg_price_monthyear, aes(x = avg_price_monthyear$monthyear, y = avg_price_monthyear$price, group = 1)) + 
  geom_point() + geom_line() + theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#############
#4

county_room_year = housing %>%
  dplyr::select(county, br, date, price) %>%
  group_by(year_sold = as.integer(format(as.Date(date), "%Y"))) %>%
  filter(year_sold < 2006) %>%
  mutate(avg_price_year = mean(price)) %>%
  mutate(br = ifelse(br > 3, "4+", br)) %>%
  na.omit()

plot_list = ggplot(county_room_year, aes(x = br, y = avg_price_year, group = county, color = county))
plot_list + facet_wrap(county~year_sold, scales = 'free') + geom_point() + geom_line() + 
  labs(title = "Bedroom Count vs Price vs Year", x = "Bedroom Count", y = "Price")

# plot_list2 = ggplot(county_room_year, aes(x = year, y = avg_price_year, group = county, color = county))
# plot_list2 + facet_wrap(~br)

############
#5

housing %>%
  group_by(city) %>% 
  filter(n_distinct(county) > 1) %>% 
  distinct(city)  %>%
  pull(city)

which((housing$city == "Vallejo" & housing$county != "Solano County"))

############
#6

bsqft_data = housing %>%
  filter(bsqft < 20000, price > 0)

plot(bsqft_data$bsqft, bsqft_data$price)
price_bsqft = lm(price ~ bsqft, bsqft_data);abline(price_bsqft)
#data looks nonlinear, 

par(mfrow = c(2,2))
plot(price_bsqft)
#Residuals vs Fitted plot shows that the graph is not linear
#normal qq appears to be heavy tailed larger prob of getting large values (maybe outliers) indicates not normal
#scale-location shows that the variance is not equal
#residuals vs leverage shows that there a quite a few outliers
dev.off()

bc = boxcox(price_bsqft, lambda = seq(-5, 5, 1/10), plotit = T)
bc$x[which(bc$y == max(bc$y))]
#because the boxcox indicates a 0 lambda, that means you should use a log transformation

price2 = log(bsqft_data$price)
plot(bsqft_data$bsqft, price2)
transformed_model = lm(y2~bsqft_data$bsqft); abline(model)

par(mfrow = c(2,2))
plot(transformed_model)
dev.off()

############
#7

bsqft_lsqft_model = lm(price~bsqft + lsqft, housing)
bsqft_lsqft_sum = summary(bsqft_lsqft_model)
str(bsqft_lsqft_sum)


