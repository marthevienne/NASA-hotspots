cell_long_seg <- long_seg_dives %>%
  filter(!is.na(season)) %>%
  select(c(REF, NUM, DE_DATE, mean_depth, time_longer_seg, pol)) %>%
  filter(pol == 21) %>%
  mutate(month = month(DE_DATE),
         yday = as.numeric(DE_DATE))
 
range(cell_long_seg$yday)

cell_long_seg$bins <- cut(cell_long_seg$mean_depth, 
                          breaks = seq(0, 1000, 20),
                          include.lowest = TRUE) 

range(cell_long_seg$yday)
cell_long_seg$period <- cut(cell_long_seg$yday, 
                          breaks = seq(1237256018, 1515962606, 1e6),
                          include.lowest = TRUE) 

as.Date(1237256018, origin = "1970/01/01")
labels_bins = rev(sort(unique(cell_long_seg$bins)))

cell_long_seg$bins <- as.numeric(cell_long_seg$bins)
cell_long_seg$period <- as.numeric(cell_long_seg$period)

n_season <- cell_long_seg %>%
  group_by(pol, period) %>%
  mutate(nseason = n()) %>%
  ungroup() %>%
  group_by(pol, period, bins) %>%
  mutate(nbin = n()) %>%
  reframe(frac = nbin/nseason, n = nbin) %>%
  unique()

str(cell_long_seg)
heatmap <- ggplot(data = n_season, aes(x = period, y = -bins, z = frac, fill = frac)) +
  geom_contour()

heatmap_long_seg <- ggplot(data = n_season, aes(x = period, y = -bins, z = frac, fill = frac)) +
  geom_tile() +
  geom_contour(col = "white", linewidth = .1)+
  #xlab("Polynya ids") +
  ylab("Mean depth of longest segments (m)") +
  #scale_y_discrete(labels = labels_bins) #+
  scale_fill_gradient("Fraction",
                       limit = c(0,0.8)) +
  theme_bw()

heatmap_long_seg

nobs <- n_season %>%
  group_by(pol, month) %>%
  reframe(n_obs = paste0("n=", sum(n)),
          y = 1)

nobs$month <- factor(nobs$season, levels = c("Summer", "Autumn", "Winter"))
n_season$season <- factor(n_season$season, levels = c("Summer", "Autumn", "Winter"))

heatmap_long_seg <- ggplot() +
  geom_tile(data = n_season, aes(x = factor(pol), y = factor(-bins), fill = frac)) +
  geom_text(data = nobs, aes(x = factor(pol), y = y, label = n_obs), 
            color = "darkgrey", size = 3,  angle = 90, hjust = 0) +
  facet_grid(~season) +
  xlab("Polynya ids") +
  ylab("Mean depth of longest segments (m)") +
  scale_y_discrete(labels = labels_bins) +
  scale_fill_gradient2("Fraction", low = "white", high = "darkblue", mid = "beige",
                       midpoint = 0.05, limit = c(0,0.73)) +
  theme_bw()