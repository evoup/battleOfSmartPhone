library(ggplot2)
#library(plyr)
#library(magrittr)
library(dplyr)
library(sqldf)
library(ggthemes)
library(gridExtra)
#library(grid)
library(leaflet)
library(viridis)

setwd("~/project/kaggle/")
app_events_df <- read.csv("input/app_events.csv", header = T, stringsAsFactors = F)
app_labels_df <- read.csv("input/app_labels.csv", header = T, stringsAsFactors = F)
event_df <- read.csv("input/events.csv", header = T, stringsAsFactors = F)
gender_age_train_df <- read.csv("input/gender_age_train.csv", header = T, stringsAsFactors = F)
label_categories_df <- read.csv("input/label_categories.csv", header = T, stringsAsFactors = F)
phone_brand_device_model_df <- read.csv("input/phone_brand_device_model.csv", header = T, stringsAsFactors = F)

raw = sqldf("SELECT phone_brand_device_model_df.device_id,
phone_brand_device_model_df.phone_brand,phone_brand_device_model_df.device_model,
gender_age_train_df.gender,gender_age_train_df.age 
FROM phone_brand_device_model_df JOIN gender_age_train_df 
            ON phone_brand_device_model_df.device_id=gender_age_train_df.device_id")

#raw_left = sqldf("SELECT raw.device_id,raw.phone_brand,raw.device_model,raw.gender,raw.age,
#event_df.longitude,event_df.latitude FROM event_df LEFT JOIN raw ON event_df.device_id=raw.device_id")

#raw_right = sqldf("SELECT raw.device_id,raw.phone_brand,raw.device_model,raw.gender,raw.age,
#event_df.longitude,event_df.latitude FROM raw LEFT JOIN event_df ON event_df.device_id=raw.device_id")

raw_all = sqldf("SELECT raw.device_id device_id,raw.phone_brand phone_brand,raw.device_model device_model,
raw.gender gender,raw.age age,event_df.longitude longitude,event_df.latitude latitude 
FROM event_df LEFT JOIN raw ON event_df.device_id=raw.device_id 
                UNION SELECT raw.device_id,raw.phone_brand,raw.device_model,raw.gender,raw.age,
event_df.longitude,event_df.latitude FROM raw LEFT JOIN event_df ON event_df.device_id=raw.device_id")


data1 <- raw[!duplicated(raw$device_id),]
brand <- c("HTC", "LG","OPPO","vivo","三星","中兴","乐视","努比亚","华为","小米","索尼","联想","酷派","金立","魅族")

eng.brand <- c("HTC", "LG","OPPO","vivo","Samsung","ZTE","LeEco","Niube","Huawei","Xiaomi","Sony","Lenovo","Kupo","Gionne","Meizu")

data1$phone_brand <- plyr::mapvalues(as.factor(data1$phone_brand), brand, eng.brand)

top15 <- names(sort(table(data1[]$phone_brand), decreasing = T))[1:15]
top5 <- names(sort(table(data1$phone_brand), decreasing = T))[1:5]


data1 %>%
  group_by(gender, age, phone_brand) %>%
  summarise(n=n()) %>%
  filter(phone_brand %in% top15) -> gapn

g1 <- ggplot(data=gapn, aes(x=reorder(phone_brand,n), y=n, fill = gender)) + 
  geom_bar(stat = "identity", position="dodge",alpha=.7) + 
  scale_fill_brewer(palette = "Set1") + 
  # scale_y_continuous(labels=function(x) x/1000) + 
  xlab("") + 
  ylab("# of Users") + 
  coord_flip() + 
  theme_tufte(base_size = 10) + 
  theme(text = element_text(family = "simkai"), legend.position="top", legend.title=element_blank())

g2 <- ggplot(data=gapn, aes(x=reorder(phone_brand,n), y=n, fill = gender)) + 
  geom_bar(stat = "identity", position="fill",alpha=.7) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_y_continuous(labels=function(x) round(x,2)) + 
  xlab("") + 
  ylab("% of Users") + 
  coord_flip() + 
  theme_tufte(base_size = 10) + 
  theme(text = element_text(family = "simkai"), legend.position="top", legend.title=element_blank())


grid.arrange(g1, g2, nrow = 1)



data1 %>% 
  group_by(phone_brand, gender, age) %>%
  summarise(n=n()) %>%
  filter(phone_brand %in% top15) -> pgan

ggplot(data = pgan, aes(x=reorder(phone_brand, age), y=age)) + 
  facet_grid(gender~.) + 
  geom_boxplot() + 
  geom_jitter(alpha = .2, shape = 1, width = .5) + 
  xlab("") + ylab("age") + 
  theme_tufte(base_size = 10) +
  theme(legend.position="none") + 
  theme(text = element_text(family = "simkai"))



g1 <- ggplot(data = filter(pgan, phone_brand %in% top5), 
             aes(x=age, y=n, fill=reorder(phone_brand,-n))) + 
  geom_bar(stat = "identity", col = "white", size = 0.1, position = "stack", alpha=.7) + 
  scale_fill_brewer(palette = "Set1") + 
  xlim(15, 55) + 
  ylab("# of Users") + 
  facet_grid(~gender, drop = T) +
  theme_tufte(base_size = 10) +
  theme(legend.title = element_blank()) + 
  theme(text = element_text(family = "simkai"), legend.position="top")

g2 <- ggplot(data = filter(pgan, phone_brand %in% top5), 
             aes(x=age, y=n, fill=reorder(phone_brand,-n))) + 
  geom_bar(stat = "identity", col = "white", size = 0.1, position = "fill", alpha=.7) + 
  scale_fill_brewer(palette = "Set1") + 
  xlim(15, 55) + 
  ylab("% of Users") + 
  scale_y_continuous(labels=function(x) round(x,2)) + 
  facet_grid(~gender, drop = T) +
  theme_tufte(base_size = 10) +
  theme(legend.title = element_blank()) + 
  theme(text = element_text(family = "simkai"), legend.position="top")

grid.arrange(g1, g2, nrow = 1)


data1 <- raw_all[!duplicated(raw_all$device_id),]
data1$phone_brand <- plyr::mapvalues(as.factor(data1$phone_brand), brand, eng.brand)
data1 %>% 
  filter(phone_brand %in% top5, !is.na(longitude), !is.na(latitude)) %>%
  sample_frac(.5) -> data2

data2$phone_brand <- droplevels(data2$phone_brand)

factpal <- colorFactor(viridis(5), data2$phone_brand)
qpal <- colorNumeric(viridis(10, option = "plasma"), data2$age)
factpal2 <- colorFactor(c("darkorange", "steelblue"), data2$gender)

leaflet(data = data2) %>% 
  setView(lng = 103, lat = 35, zoom = 4) %>%
  addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(stroke = FALSE, group = "Brand", 
                   fillOpacity = 0.5, 
                   radius=2.5, 
                   popup = ~phone_brand, 
                   color = ~factpal(data2$phone_brand)) %>%
  addCircleMarkers(stroke = FALSE, group = "Age", 
                   fillOpacity = 0.5, 
                   radius=2.5, 
                   popup = ~age, 
                   color = ~qpal(data2$age)) %>%
  addCircleMarkers(stroke = FALSE, group = "Gender", 
                   fillOpacity = 0.5, 
                   radius=2.5, 
                   popup = ~phone_brand, 
                   color = ~factpal2(data2$gender)) %>%
  addLegend("topleft", pal = factpal, values = data2$phone_brand, 
            title = "Phone brands", 
            opacity = .8) %>% 
  addLegend("topleft", pal = qpal, values = data2$age, 
            title = "User age", 
            opacity = .8) %>% 
  addLegend("topleft", pal = factpal2, values = data2$gender, 
            title = "User gender", 
            opacity = .8) %>% 
  addLayersControl(
    baseGroups = c("Brand", "Age", "Gender"),
    # overlayGroups = c(),
    options = layersControlOptions(collapsed = FALSE)
  )


