plot_dive <- function(ref, num) {
  dive <- readRDS("~/Desktop/WHOI/Data/behavioural_data/dives_profiles") %>%
    filter(REF == ref & NUM == num) %>%
    group_by(REF, NUM) %>%
    mutate(dur = (time - nth(time, 1))/60,
           speed = abs(c(diff(depth), NA)) / c(diff(time), NA))
  
  p <- ggplot() +
    geom_point(data = dive, aes(x = dur, y = -depth, group = interaction(REF, NUM))) +
    geom_path(data = dive, aes(x = dur, y = -depth, group = interaction(REF, NUM))) +
    theme_bw()
  
  print(p)
}


## Test
# ref = "ct98-47-13"
# num = 3330
# 
# plot_dive(ref, num)
