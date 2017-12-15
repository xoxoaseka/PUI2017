setwd("C:\\Users\\ad4336\\Documents\\Yelp_data_processing")
#install.packages("reshape2")
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(reshape2)
########review file#########
#slow
#yelp_reviews <- fread("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\review.json", fill = TRUE)
#faster
reviews <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\review.json"), pagesize = 10000)
head(reviews, 10) 
str(reviews)
reviews_flat <- flatten(reviews) #making it prettier
str(reviews_flat)
reviews_tbl <- as_data_frame(reviews_flat)
reviews_tbl
reviews_subset <- reviews_tbl %>% select(-starts_with("text"), -starts_with("useful"), -starts_with("funny"), -starts_with("cool")) 
reviews_2017 <- reviews_subset %>% filter(str_detect(date, "2017"))
head(reviews_2017)

########business file#########
business <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\business.json")) #importing json 
head(business, 10) 
str(business)
business_flat <- flatten(business) #making it prettier
str(business_flat)
business_tbl <- as_data_frame(business_flat)
business_tbl
#Removing unnecessary variables and selecting only restaurant reviews
business_subset <- business_tbl %>% select(-starts_with("hours"), -starts_with("attribute"), 
                                          -starts_with("is_open"), -starts_with("neighborhood")) %>% 
                                    filter(str_detect(categories, "Restaurant"))
#looking at number of reviews by cities
business_cities <- business_subset %>% count(city) %>% arrange(desc(n))
#extracting data for top two cities with number of reviews > 5K
business_toronto <- business_subset %>% filter(str_detect(city, "Toronto"))
business_lasVegas <- business_subset %>% filter(str_detect(city, "Las Vegas"))

#########toronto############

total_toronto <- merge(reviews_2017, business_toronto, by="business_id")
total_toronto <- total_toronto %>% select(starts_with("business_id"), starts_with("user_id"), starts_with("stars.x")) 
toronto_matrix <- acast(total_toronto,user_id~business_id)
sum(is.na(toronto_matrix))
write.csv(toronto_matrix, file = "TorontoRatings.csv")

#########lasVegas############

total_lasVegas <- merge(reviews_2017, business_lasVegas, by="business_id")
total_lasVegas <- total_lasVegas %>% select(starts_with("business_id"), starts_with("user_id"), starts_with("stars.x")) 
lasVegas_matrix <- acast(total_lasVegas,user_id~business_id)
sum(is.na(lasVegas_matrix))
dim(lasVegas_matrix)
write.csv(lasVegas_matrix, file = "LasVegasRatings.csv")
