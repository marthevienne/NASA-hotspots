dives_table = readRDS("dives_table")
ref = dives_table$REF[1]

num = seq(3090, 3100)

dive = dives_table[which(dives_table$REF == ref & dives_table$NUM %in% num), ]
which(dive$depth == max(dive$depth))
dive$depth[which(dive$depth == 0)] = 5

dives_plot = ggplot(dive, aes(x = as_datetime(time), y = depth)) +
  geom_point() + 
  scale_y_reverse() +
  facet_wrap(~NUM, scale = 'free')+
  labs(x="Time", y="Depth")+
  ggtitle(ref)+
  theme(axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=17,face="bold"), #__________ legend texts
        legend.title=element_text(size=17,face="bold"))


ggsave("exemple_dives.png",height=20,width=30,units=c("cm"),dpi=300)