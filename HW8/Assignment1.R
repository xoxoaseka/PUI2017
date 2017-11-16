setwd("C:\\Users\\ad4336\\Documents\\Yelp_data_processing")
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
#library(scales)
#library(grid)
#library(RColorBrewer)
########review file#########
reviews <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\review.json"), pagesize = 10000)
#head(reviews, 10) 
str(reviews)
reviews_flat <- flatten(reviews)
str(reviews_flat)
reviews_df <- as_data_frame(reviews_flat)
reviews_subset <- reviews_df %>% select(-starts_with("text"), -starts_with("useful"), -starts_with("funny"), -starts_with("cool")) 
########business file#########
business <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\business.json")) #importing json 
#head(business, 10) 
str(business)
business_flat <- flatten(business)
str(business_flat)
business_df <- as_data_frame(business_flat)
#selecting only restaurant reviews
business_subset <- business_df %>% select(-starts_with("hours"), -starts_with("attribute"), -starts_with("is_open"), -starts_with("neighborhood")) %>% 
  filter(str_detect(categories, "Restaurant"))

#looking at number of reviews by cities
business_cities <- business_subset %>% count(city) %>% arrange(desc(n))
#extracting data for top two cities with # of reviews > 5K
business_toronto <- business_subset %>% filter(str_detect(city, "Toronto"))
business_lasVegas <- business_subset %>% filter(str_detect(city, "Las Vegas"))

#########lasVegas############
lasVegas <- merge(reviews_subset, business_lasVegas, by="business_id")
lasVegas <- lasVegas %>% select(starts_with("business_id"), starts_with("date"), starts_with("stars.x")) %>% arrange(business_id)
#lasVegas_matrix <- as.matrix(lasVegas)
#write.csv(lasVegas_matrix, file = "LasVegas_test.csv")
lasVegas_2008 <- lasVegas %>%
  filter(substr(date,1,4) == 2008) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))


#colors = c("#16a085","#27ae60","#2980b9","#8e44ad","#f39c12","#c0392b","#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#f1c40f","#e74c3c")

#rank_colors = c(brewer.pal(9, "Reds")[c(8,7,6)],brewer.pal(9, "Greens")[c(7,8)])

ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=lasVegas_2008) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(lasVegas_2008$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(lasVegas_2008$ymin + lasVegas_2008$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank(), legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="Ring plot of Las Vegas restaurant ratings", subtitle = "A subtitle", caption = "(based on data from ...)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Las Vegas 2008", size=8)
ggsave("lasVegas-2008.png", dpi=300, width=3, height=3)


lasVegas_2016 <- lasVegas %>%
  filter(substr(date,1,4) == 2016) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=lasVegas_2016) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(lasVegas_2016$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(lasVegas_2016$ymin + lasVegas_2016$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank(), legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="Ring plot of Las Vegas restaurant ratings", subtitle = "A subtitle", caption = "(based on data from ...)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Las Vegas 2016", size=8)
ggsave("lasVegas-2016.png", dpi=300, width=3, height=3)

#########toronto############

toronto <- merge(reviews_subset, business_toronto, by="business_id")
toronto <- toronto %>% select(starts_with("business_id"), starts_with("date"), starts_with("stars.x")) %>% arrange(business_id)


toronto_2008 <- toronto %>%
  filter(substr(date,1,4) == 2008) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))


#colors = c("#16a085","#27ae60","#2980b9","#8e44ad","#f39c12","#c0392b","#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#f1c40f","#e74c3c")

#rank_colors = c(brewer.pal(9, "Reds")[c(8,7,6)],brewer.pal(9, "Greens")[c(7,8)])

ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=toronto_2008) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(toronto_2008$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(toronto_2008$ymin + toronto_2008$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank(), legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="Ring plot of Las Vegas restaurant ratings", subtitle = "A subtitle", caption = "(based on data from ...)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Toronto 2008", size=8)
ggsave("toronto-2008.png", dpi=300, width=3, height=3)


toronto_2016 <- toronto %>%
  filter(substr(date,1,4) == 2016) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))


#colors = c("#16a085","#27ae60","#2980b9","#8e44ad","#f39c12","#c0392b","#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#f1c40f","#e74c3c")

#rank_colors = c(brewer.pal(9, "Reds")[c(8,7,6)],brewer.pal(9, "Greens")[c(7,8)])

ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=toronto_2016) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(toronto_2016$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(toronto_2016$ymin + toronto_2016$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),
        legend.title = element_blank(), legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(title="Ring plot of Las Vegas restaurant ratings", subtitle = "A subtitle", caption = "(based on data from ...)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Toronto 2016", size=8)
ggsave("toronto-2016.png")


#############################################

require(gridExtra)

plot1 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=lasVegas_2008) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(lasVegas_2008$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(lasVegas_2008$ymin + lasVegas_2008$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(), legend.position="none") +
  scale_fill_manual(values=rank_colors) +
  scale_color_manual(values=rank_colors) +
  annotate("text", x = 0, y = 0, label = "Las Vegas (2008)", size=4)

plot2 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=toronto_2008) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(toronto_2008$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(toronto_2008$ymin + toronto_2008$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(), legend.position="none") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Toronto (2008)", size=4)


plot3 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=lasVegas_2016) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(lasVegas_2016$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(lasVegas_2016$ymin + lasVegas_2016$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),legend.position="none") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Las Vegas (2016)", size=4)


plot4 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=toronto_2016) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(toronto_2016$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(toronto_2016$ymin + toronto_2016$ymax)/2, size=3, col = rank_colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),legend.position="none") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = 0, y = 0, label = "Toronto (2016)", size=4)+
  labs(subtitle = "A subtitle", caption = "(https://www.yelp.com/dataset)")


grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow =2, top = "Proportions of 1 to 5 star ratings in restaurant reviews from Yelp")







