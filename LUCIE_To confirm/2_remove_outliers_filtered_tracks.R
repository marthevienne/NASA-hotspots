rm(list=ls())

filtered_tracks <- read.csv("C:/Users/lucie/Dropbox/codes/ssm/filteredTracks_ssm_polynya_updated.csv", stringsAsFactors = F)
filtered_tracks$date=as.POSIXct(filtered_tracks$date,origin="1970-01-01",tz="GMT")
str(filtered_tracks)

## Tracks to delete

ind <- c("ct34-2444-08","ct75-020-11","ct75-030-11")

ToDelete1 <- which(filtered_tracks$id==ind[1])
ToDelete2 <- which(filtered_tracks$id==ind[2])
ToDelete3 <- which(filtered_tracks$id==ind[3])

filtered_tracks_delete <- filtered_tracks[-c(ToDelete1,ToDelete2,ToDelete3),]

## Tracks to modify

# IND 1 "ct47-B-09"
filtered_tracks_delete <- filtered_tracks_delete[-which(filtered_tracks_delete$id == "ct47-B-09" & filtered_tracks_delete$date > "2009-07-05 04:00:00 GMT"),]

# IND 2 "ct47-E-09"
filtered_tracks_delete <- filtered_tracks_delete[-which(filtered_tracks_delete$id == "ct47-E-09" & filtered_tracks_delete$date > "2009-05-06 20:00:00 GMT"),]

# IND 3 "ct47-I-09"
filtered_tracks_delete <- filtered_tracks_delete[-which(filtered_tracks_delete$id == "ct47-I-09" & filtered_tracks_delete$date > "2009-06-22 03:00:00 GMT"),]

# IND 4 "ct75-838-11"
filtered_tracks_delete <- filtered_tracks_delete[-which(filtered_tracks_delete$id == "ct75-838-11" & filtered_tracks_delete$date < "2011-11-10 08:00:00 GMT"),]

# IND 5 "ct75-839-11"

ind5 <- filtered_tracks_delete[which(filtered_tracks_delete$id == "ct75-839-11"),]



ind6 <- filtered_tracks_delete[which(filtered_tracks_delete$id == "ct77-167-12"),]

ind7 <- filtered_tracks_delete[which(filtered_tracks_delete$id == "ct77-170-12"),]











