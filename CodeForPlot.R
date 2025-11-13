# data cleaning ####

library(tidyverse)
library(ggplot2)
library(dplyr)
forest <- forest_change_Pink_Lake
forest_long <- forest %>%
  pivot_longer(
    cols = c(starts_with("gain_"), starts_with("loss")),
    names_to = "name",
    values_to = "area_km2")%>%
  separate(col = name,
           into = c("type", "year"),
           sep = "_") %>%
  mutate(
    year=as.integer(year))

forest_long %>%
  select(NAME, year, type, area_km2) %>%
  arrange(NAME, year, type)

forest_tid <- forest_long %>%
  pivot_wider(
    names_from = type,
    values_from = area_km2
  )%>%
  mutate(
    loss = if_else(is.na(loss), 0, loss),
    gain = if_else(is.na(gain), 0, gain),
    net_change_km2 = gain - loss) %>%
  arrange(NAME, year)

forest_tid <- forest_tid %>%
  select(NAME, year, loss, gain, net_change_km2)

# making a cumulative column

forest_tidy <- forest_tid %>% mutate(cumulativeloss = -1*cumsum(net_change_km2)- gain) %>% mutate(Status = "Present")


# data visualisation ####

install.packages("wesanderson")
# Load
library(wesanderson)

ggplot(forest_tidy, aes(x = year,y = loss))+
  geom_col()

ggplot(forest_tidy, aes(x = year,y = cumulativeloss))+
  geom_col(fill="darkgreen", color = "black", alpha = 0.8)+
  geom_smooth(se=F)+
  labs(y = "Total Forest Area Lost (km^2)",
         x = "Year")+
  theme_minimal()

future_years<- tibble(year=2017:2027)

prediction_data <- future_years %>%  mutate(cumulativeloss = predict(mod1, future_years)) %>% mutate(Status="Future")

mod1<-(lm(cumulativeloss~year, data = forest_tidy))

ggplot(prediction_data, aes(x = year, y = predicted_loss) )+
  geom_col(fill="darkgreen", color = "black", alpha = 0.8)+
  geom_smooth(se=F)+
  labs(y = "Total Forest Area Lost (km^2)",
       x = "Year")+
  theme_minimal()

df<-data.frame()


forest_new <- forest_tidy %>% select( year,cumulativeloss, Status)

forest_tidy<- rbind(forest_new, prediction_data)

ggplot(forest_tidy, aes(x = year,y = cumulativeloss ))+
  geom_col(aes(fill = Status) )+
  geom_smooth(se=F, color = "darkgreen")+
  scale_fill_manual(values = wes_palette("Royal2", n = length(unique(forest_tidy$Status)), type = "discrete")) +
  labs(y = "Total Forest Area Lost (km^2)",
       x = "Year")+
  theme_minimal()

