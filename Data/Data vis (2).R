forest_tidy <- forest_tidy %>%
  pivot_longer(
    cols = c(loss, gain),
    names_to = "type",
    values_to = "area_km2"
  )

ggplot(forest_tidy, aes(x = year, y = area_km2, fill = type)) +
  geom_col(position = "dodge") + 
  labs(title = "Forest cover loss and gain in Pink Lake (2001-2016)",
       x = "Year", 
       y = "Area (km2)",
       fill= "Type")+
  theme_minimal()

ggplot(forest_tidy, aes(x = year, y = net_change_km2))+
  geom_line()+
  geom_point()+
  labs(
    title = "Net forest cover change (gain - loss) in Pink Lake",
    x = "Year",
    y = "Net change (km2)"
  )+ theme_minimal()

