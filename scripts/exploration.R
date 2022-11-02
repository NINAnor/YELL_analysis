library(RSQLite)
library(DBI)
library(tidyverse)


sqlite <- dbDriver("SQLite")
conn_birdnet <- dbConnect(sqlite, "/home/rstudio/app/data/birdnet.sqlite")
conn_snowscooter <- dbConnect(sqlite, "/home/rstudio/app/data/snowscooter.db")

# Fetch birdnet results
res_bn <- dbSendQuery(conn_birdnet, "SELECT * FROM birdnet WHERE Confidence >= 0.6")
birdnet <- dbFetch(res_bn)

# Fetch snowscooter results
res_sn <- dbSendQuery(conn_snowscooter, "SELECT * FROM [/home/benjamin.cretois/Code/mloutput2sql/my_analysis/yellowstone/YELL_db.db] WHERE Confidence >= 0.99")
sn <- dbFetch(res_sn)


##################
# DATA WRANGLING #
##################

sn$full_date = as.Date(paste(sn$date, sn$time_detection), format="%Y-%m-%d %H:%M:%S") # Maybe as.POSIXT is better?
sn_clean <- sn %>% 
  select(confidence, location, full_date, filename) %>%
  mutate(label = "snowscooter") %>% 
  mutate("common_name" = "snowscooter")

birdnet$full_date = as.Date(paste(birdnet$date, birdnet$time_detection), format="%Y-%m-%d %H:%M:%S")
bn_clean <- birdnet %>% 
  mutate(label = "bird") %>% 
  select(confidence = Confidence, location, full_date, filename, label, common_name = `Common Name`)
  
prediction_df <- rbind(sn_clean, bn_clean)

head(prediction_df)

#############################
# PLOTS // DATA EXPLORATION #
#############################

# ! HAVE A WIND INDEX AS A COVARIATE !

prediction_df %>% 
  group_by(full_date, location, label) %>% 
  summarise(n = n()) %>%
  group_by(label, location) %>% 
  mutate(n_perc = n / sum(n)) %>% 
  ggplot(aes(x=full_date, y=n_perc, col=label)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~location, scales="free")
# Need to normalise the number of predictions (to have btw 0 and 1)

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





######################
# Quick linear model #
######################

sn_det <- full_join(sp, sn, by=c("full_date", "location")) 

v <- sn_det$n.y
v[is.na(v)] <- 0

sn_det$snowscooter_det <- v

summary(lm(n.x ~ snowscooter_det, data = sn_det))
