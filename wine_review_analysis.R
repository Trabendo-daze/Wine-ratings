#This script analyzes the data set of 130k wine reviews posted by user "zynicide" on Kaggle.com
#This project is intended as a first foray into show-casing my R capabilities, focusing on 
#basic data analysis using style conventions from Hadley Wickham's tidyverse. I will be replicating
#this code as a markdown file as well.

#---------Loading in packages and the data--------------

library(tidyverse)
wine130k <- "data_repository\\winereviews130k.csv"
wine_reviews <- read_csv(wine130k)


#-----------Start Data Exploration--------------

#To see what the data set looks like, we'll look at a few exploratory features

head(wine_reviews)

#Each row is a bottle of wine with features such as its country of origin and privince/region
#as well as the name of the taster, the variety of wine, its rating, a description, and its price.


#One thing that seems most interesting is seeing whether a wine's rating correlates to its price. 
#Do expensive wines tend to be rated higher?

price_vs_rating <- ggplot(wine_reviews, aes(points, price))

price_vs_rating + geom_point(color = "blue", size = 2) +
  theme_classic() + 
  labs(x = "Rating (points)", y = "Price ($)", title = "Wine Ratings vs. Price")

#At first glance, there seems to be some relationship between the two variables. However, there are some
#odd data points, such as the most expensive wine bottle which only has a rating in the high 80s. We can
#see the correlation coefficient between these two variables with the following, using the "complete.obs"
#argument to tell R to disregard bottles with NA in price or points

cor(wine_reviews$points, wine_reviews$price, use = "complete.obs")

#correlation of r = 0.42 shows a medium-strength positive correlation between Rating and Price

#Something else that seems interesting is knowing which countries have the highest rating 
#wines. We can figure this out easily using the dplyr packages to sort the data. Relevant to this 
#question also is how many wines were rated from each country

ratings_by_country <- wine_reviews %>%
  group_by(country) %>%
  summarise(number_of_wines = n(), mean_rating = mean(points)) %>%
  arrange(desc(mean_rating))

#Perhaps shockingly, French wines are rated behind those from England, Austria, and Germany, although
#England only had 74 wines rated. These differences are, however, small. Let's visualize the ratings
#of the countries with the 10 most wines rated.

most_wines_by_country_top_ten <- wine_reviews %>%
  group_by(country) %>%
  summarise(number_of_wines = n(), mean_rating = mean(points)) %>%
  arrange(desc(number_of_wines)) %>%
  head(10) %>%
  arrange(desc(mean_rating))

#Among these countries we find that Austria wins out, followed by Germany and France. The differences are small though
#so let's see them visualized.

wines_by_country_bar <- ggplot(most_wines_by_country_top_ten, aes(x = reorder(country, desc(mean_rating)), y = mean_rating, fill = country))

wines_by_country_bar + geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Mean Rating", title = "Wine Ratings of Top 10 Countries") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_brewer(palette="RdBu") + 
  ylim(0, 100)
