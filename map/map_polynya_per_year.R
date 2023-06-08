pol = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

mertz = subset(pol, ID == 21 & month == "July")

ggplot() +
  geom_polygon(data = mertz, aes(x = lon, y = lat, group = num), col = "black", fill = "NA") +
  facet_grid(~year)+
  theme(legend.position = "none")
