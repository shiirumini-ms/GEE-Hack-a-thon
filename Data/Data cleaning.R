library(tidyverse)
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

forest_tidy <- forest_long %>%
  pivot_wider(
    names_from = type,
    values_from = area_km2
  )%>%
  mutate(
    loss = if_else(is.na(loss), 0, loss),
    gain = if_else(is.na(gain), 0, gain),
    net_change_km2 = gain - loss) %>%
  arrange(NAME, year)

forest_tidy <- forest_tidy %>%
  select(NAME, year, loss, gain, net_change_km2)

write.csv(forest_tidy, "forest_tidy.csv")