library(RSQLite)
library(DBI)
library(tidyverse)
install.packages("chron")
library(chron)
library(lubridate)

# See floor_date for aggregating per hours
# group_by(Date=floor_date(DateTime, "1 hour"))


sqlite <- dbDriver("SQLite")
conn_birdnet <- dbConnect(sqlite, "/home/rstudio/app/data/birdnet.sqlite")
conn_snowscooter <- dbConnect(sqlite, "/home/rstudio/app/data/snowscooter.db")

# Fetch birdnet results
res_bn <- dbSendQuery(conn_birdnet, "SELECT * FROM birdnet WHERE Confidence >= 0.6")
birdnet <- dbFetch(res_bn)

# Fetch snowscooter results
res_sn <- dbSendQuery(conn_snowscooter, "SELECT * FROM [/home/benjamin.cretois/Code/mloutput2sql/my_analysis/yellowstone/YELL_db.db] WHERE Confidence >= 0.99")
sn <- dbFetch(res_sn)

sn$date
##################
# DATA WRANGLING #
##################
sn$full_date = as.POSIXct(paste(sn$date, sn$time_detection), format="%Y-%m-%d %H:%M:%S") # Maybe as.POSIXT is better?
sn$full_date
sn_clean <- sn %>% 
  select(confidence, location, full_date, filename) %>%
  mutate(label = "snowscooter") %>% 
  mutate("common_name" = "snowscooter")

birdnet$full_date = as.POSIXct(paste(birdnet$date, birdnet$time_detection), format="%Y-%m-%d %H:%M:%S")
bn_clean <- birdnet %>% 
  mutate(label = "bird") %>% 
  select(confidence = Confidence, location, full_date, filename, label, common_name = `Common Name`)
  
prediction_df <- rbind(sn_clean, bn_clean)

# Add other informative columns
prediction_df <- prediction_df %>% mutate(is_weekend = is.weekend(prediction_df$full_date))

head(prediction_df)
#############################
# PLOTS // DATA EXPLORATION #
#############################

# ! HAVE A WIND INDEX AS A COVARIATE !

# Focusing on a single site YELLBTBC
prediction_df %>% 
  #filter(location == "YELLBTBC") %>% 
  group_by(full_date, location, label) %>% 
  summarise(n = n()) %>%
  group_by(label, location) %>% 
  mutate(n_perc = n / sum(n)) %>% 
  ggplot(aes(x=full_date, y=n_perc, col=label)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~location, scales="free")
# Need to normalise the number of predictions (to have btw 0 and 1)

prediction_df

############
# WEEKDAYS #
############
prediction_df %>% 
  filter(location == "YELLBTBC") %>% 
  filter(is_weekend == FALSE) %>% 
  group_by(location, label, Date=as.POSIXct(floor_date(full_date, "2 hour"))) %>% 
  summarise(n = n()) %>%
  group_by(label, location) %>% 
  mutate(n_perc = n / sum(n)) %>%  
  mutate(id = as.integer(factor(floor_date(Date, "1 week")))) %>% 
  ggplot(aes(x=Date, y=n_perc, col=label)) +
  geom_point() +
  geom_boxplot(aes(col=label, group=interaction(label,id), alpha=.5)) + 
  #geom_line() +
  scale_x_datetime(breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


############
# WEEKENDS #
############

prediction_df %>% 
  filter(location == "YELLBTBC") %>% 
  filter(is_weekend == TRUE) %>% 
  group_by(location, label, Date=as.POSIXct(floor_date(full_date, "1 hour"))) %>% 
  summarise(n = n()) %>%
  group_by(label, location) %>% 
  mutate(n_perc = n / sum(n)) %>%  
  mutate(id = as.integer(factor(floor_date(Date, "2 days")))) %>% 
  ggplot(aes(x=Date, y=n_perc, col=label)) +
  geom_point() +
  geom_boxplot(aes(col=label, group=interaction(label,id), alpha=.5)) + 
  #geom_line() +
  scale_x_datetime(breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ONLY SNOWSCOOTERS
############
# WEEKENDS #
############

prediction_df %>% 
  filter(location == "YELLCRPA") %>% 
  filter(label == "snowscooter") %>% 
  #filter(is_weekend == TRUE) %>% 
  group_by(is_weekend, location, label, Date=as.POSIXct(floor_date(full_date, "1 hour"))) %>% 
  summarise(n = n()) %>%
  group_by(label, location) %>% 
  mutate(n_perc = n / sum(n)) %>%  
  mutate(id = as.integer(factor(floor_date(Date, "1 days")))) %>% 
  ggplot(aes(x=Date, y=n,col=is_weekend)) +
  geom_point() +
  geom_boxplot(aes(col=is_weekend, group=interaction(is_weekend,id), alpha=.5), show.legend = FALSE) + 
  #geom_line() +
  scale_x_datetime(breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Number of detections") +
  labs(col = "Weekend")

############################################
# species richness vs number of detections #
############################################

prediction_df %>% distinct(common_name) %>% summarise(n = n()) # 218 unique species

sp <- prediction_df %>%
  filter(label == "bird") %>% 
  group_by(full_date, label, location) %>% 
  distinct(common_name) %>% 
  summarise(n = n())%>% 
  group_by(location) %>% 
  mutate(n_perc = n / sum(n))


sn <- prediction_df %>%
  filter(label == "snowscooter") %>% 
  group_by(full_date, label, location) %>% 
  summarise(n = n()) %>% 
  group_by(location) %>% 
  mutate(n_perc = n / sum(n))

sp_sn <- rbind(sp, sn)


# Y: number of detections normalized by number of detection at the specific location
# X: the location
sp_sn %>% 
  group_by(label) %>% 
  ggplot(aes(x=location, y=n_perc, col=label)) + 
  geom_boxplot() +
  ylab("Number of detection (stand. by locations)")



################# FILTER A DATE #########################

prediction_df %>% 
  filter(location == "YELLCRPA") %>% 
  filter(label == "snowscooter") %>% 
  #filter(is_weekend == TRUE) %>% 
  mutate(Date=as.POSIXct(full_date)) %>% 
  filter(Date < as.POSIXct("2011-01-13 00:00:00")) %>% 
  group_by(Date=as.POSIXct(floor_date(full_date, "1 hour"))) %>% 
  summarise(n = n()) %>%
  #summarise(n = n()) %>%
  #group_by(label, location) %>% 
  #mutate(n_perc = n / sum(n)) %>%  
  #mutate(id = as.integer(factor(floor_date(Date, "1 hour")))) %>% 
  ggplot(aes(x=Date, y=n)) +
  geom_point() +
  #geom_boxplot(aes(col=is_weekend, group=interaction(is_weekend,id), alpha=.5), show.legend = FALSE) + 
  #geom_line() +
  scale_x_datetime(breaks = "1 hour") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Number of detections") +
  xlab("Hours")
  labs(col = "Weekend")


######################
# Quick linear model #
######################

sn_det <- full_join(sp, sn, by=c("full_date", "location")) 

v <- sn_det$n.y
v[is.na(v)] <- 0

sn_det$snowscooter_det <- v

summary(lm(n.x ~ snowscooter_det, data = sn_det))
