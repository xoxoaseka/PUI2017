setwd("C:\\Users\\ad4336\\Documents\\Yelp_data_processing")
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
########review file#########
reviews <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\review.json"), pagesize = 10000)
str(reviews)
reviews_flat <- flatten(reviews)
str(reviews_flat)
reviews_df <- as_data_frame(reviews_flat)
reviews_subset <- reviews_df %>% select(-starts_with("text"), -starts_with("useful"), -starts_with("funny"), -starts_with("cool")) 
########business file#########
business <- stream_in(file("\\Users\\ad4336\\Documents\\Yelp_data_processing\\data\\business.json")) #importing json 
str(business)
business_flat <- flatten(business)
str(business_flat)
business_df <- as_data_frame(business_flat)
#selecting only restaurant reviews
business_subset <- business_df %>% select(-starts_with("hours"), -starts_with("attribute"), 
                                          -starts_with("is_open"), -starts_with("neighborhood")) %>% 
  filter(str_detect(categories, "Restaurant"))

#looking at number of reviews by cities
business_cities <- business_subset %>% count(city) %>% arrange(desc(n))
#extracting data for top two cities with number of reviews > 5K
business_toronto <- business_subset %>% filter(str_detect(city, "Toronto"))
business_lasVegas <- business_subset %>% filter(str_detect(city, "Las Vegas"))

#########lasVegas############
lasVegas <- merge(reviews_subset, business_lasVegas, by="business_id")
lasVegas <- lasVegas %>% select(starts_with("business_id"), starts_with("date"), starts_with("stars.x")) %>% arrange(business_id)

lasVegas_2008 <- lasVegas %>%
  filter(substr(date,1,4) == 2008) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

lasVegas_2016 <- lasVegas %>%
  filter(substr(date,1,4) == 2016) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

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

toronto_2016 <- toronto %>%
  filter(substr(date,1,4) == 2016) %>%
  group_by(stars.x) %>%
  summarize(count = n()) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

##############plot################
require(gridExtra)

colors = c("tomato3", "tomato", "wheat3", "yellow3", "yellowgreen")
plot1 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=lasVegas_2008) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(lasVegas_2008$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(lasVegas_2008$ymin + lasVegas_2008$ymax)/2, size=3, col = colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(), legend.position="none") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  annotate("text", x = 0, y = 0, label = "Las Vegas, 2008", size=4)

plot3 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=toronto_2008) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(toronto_2008$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(toronto_2008$ymin + toronto_2008$ymax)/2, size=3, col = colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(), legend.position="none") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  annotate("text", x = 0, y = 0, label = "Toronto, 2008", size=4)


plot2 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=lasVegas_2016) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(lasVegas_2016$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(lasVegas_2016$ymin + lasVegas_2016$ymax)/2, size=3, col = colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),legend.position="none") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  annotate("text", x = 0, y = 0, label = "Las Vegas, 2016", size=4)


plot4 <- ggplot(aes(fill=as.factor(stars.x), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=toronto_2016) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  annotate("text", label = paste(format(toronto_2016$fraction * 100, digits=2),"%",sep=''), 
           x=rep(6,5), y=(toronto_2016$ymin + toronto_2016$ymax)/2, size=3, col = colors) +
  theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), 
        axis.title.x = element_blank(), axis.title.y=element_blank(),legend.position="none") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  annotate("text", x = 0, y = 0, label = "Toronto, 2016", size=4)


grobs <- grobTree(
  gp = gpar(fontsize = 13, fontface = "bold"), 
  textGrob(label = "Proportions of ", name = "title1",
           x = unit(0.2, "lines"), y = unit(1.4, "lines"), 
           hjust = 0, vjust = 0),
 
  textGrob(label = "1 star", name = "title2",
           x = grobWidth("title1") + unit(0.2, "lines"), y = unit(1.4, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col ="tomato3")),
  
  textGrob(label = ", ", name = "title3",
           x = grobWidth("title1") + grobWidth("title2") + unit(0.2, "lines"), y = unit(1.4, "lines"),
           hjust = 0, vjust = 0),
  
  textGrob(label = "2 stars", name = "title4",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
              unit(0.2, "lines"), y = unit(1.4, "lines"),
              hjust = 0, vjust = 0, gp = gpar(col ="tomato")),
  
  textGrob(label = ", ", name = "title5",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0),
  
  textGrob(label = "3 stars", name = "title6",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + grobWidth("title5") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col ="wheat3")),
  
  textGrob(label = ", ", name = "title7",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + grobWidth("title5") + grobWidth("title6") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0),
  
  textGrob(label = "4 stars", name = "title8",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + grobWidth("title5") + grobWidth("title6") + grobWidth("title7") + 
             unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col ="yellow3")),
  
  textGrob(label = ", ", name = "title9",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + grobWidth("title5") + grobWidth("title6") + grobWidth("title7") + 
             grobWidth("title8") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0),
  
  textGrob(label = "5 stars", name = "title10",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + grobWidth("title5") + grobWidth("title6") + grobWidth("title7") + 
             grobWidth("title8") + grobWidth("title9") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col ="yellowgreen")),
  
  textGrob(label = " ratings", name = "title11",
           x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
             grobWidth("title4") + grobWidth("title5") + grobWidth("title6") + grobWidth("title7") + 
             grobWidth("title8") + grobWidth("title9") + grobWidth("title10") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0),
  
  textGrob(label = "in restaurant reviews on Yelp", name = "title12",
           x = unit(0.2, "lines"), y = unit(0.1, "lines"),
           hjust = 0, vjust = 0)
)

grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow =2, 
             top = grobs,
             bottom = textGrob(
               "Data source: https://www.yelp.com/dataset",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1
             ),
             padding = unit(2.5, "line")
             )

