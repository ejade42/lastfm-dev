library(tidyverse)

last_fm <- read.csv("last.fm_data/last_fm_history_2026_08_10.csv") %>%
    arrange(uts)
last_fm$uts <- as_datetime(last_fm$uts, tz = Sys.timezone())
last_fm$row <- 1:nrow(last_fm)
last_fm$time_since_artist <- as.period(NA)
last_fm$time_since_track  <- as.period(NA)

for (artist in unique(last_fm$artist)) {
    artist_data <- last_fm[last_fm$artist == artist, ]
    last_fm[artist_data$row, "artist_plays"] <- nrow(artist_data)
    if (nrow(artist_data) > 1) {
        artist_data[2:nrow(artist_data), "time_since_artist"] <- as.period(artist_data[2:nrow(artist_data), "uts"] - artist_data[1:(nrow(artist_data)-1), "uts"], unit = "years")
        last_fm[artist_data$row, "time_since_artist"] <- artist_data[, "time_since_artist"]
        
        for (track in unique(artist_data$track)) {
            track_data <- artist_data[artist_data$track == track, ]
            last_fm[track_data$row, "track_plays"] <- nrow(track_data)
            if (nrow(track_data) > 1) {
                track_data[2:nrow(track_data), "time_since_track"] <- as.period(track_data[2:nrow(track_data), "uts"] - track_data[1:(nrow(track_data)-1), "uts"], unit = "years")
                last_fm[track_data$row, "time_since_track"] <- track_data[, "time_since_track"]
            }
        }
    }
}

