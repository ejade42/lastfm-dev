library(jsonlite)
library(lubridate)
library(dplyr)


## READING AND MERGING DATA
## -----------------------------------------------------------------------------
## last.fm
data_lastfm <- read.csv("last.fm data/last_fm_history_2026_08_10.csv")
data_lastfm$datetime_utc <- as_datetime(data_lastfm$uts)


## spotify
data_2015 <- read_json("2026-08-10 Spotify Extended Streaming History/Streaming_History_Audio_2015.json", simplifyVector = TRUE)
data_2015$datetime <- as_datetime(data_2015$ts)


json_to_2018 <- c("2015", "2016", "2016_1", "2017", "2017_1", "2018", "2018_1")
data_json <- NULL
for (year in json_to_2018) {
    data_year <- read_json(paste0("2026-08-10 Spotify Extended Streaming History/Streaming_History_Audio_", year, ".json"),
                           simplifyVector = TRUE)
    data_json <- bind_rows(data_json, data_year)
}

data_json$datetime_end <- as_datetime(data_json$ts)
data_json$offline_datetime_end <- as_datetime(data_json$offline_timestamp / 1000)
data_json$datetime_end_utc <- as_datetime(ifelse(is.na(data_json$offline_datetime_end) | data_json$offline_datetime_end < "2000-01-01",
                                 data_json$datetime, data_json$offline_datetime_end))
## Use time played duration to get start time, as the timestamp is end time
data_json$datetime_utc <- data_json$datetime_end_utc - dmilliseconds(data_json$ms_played)

data_json$track  <- data_json$master_metadata_track_name
data_json$artist <- data_json$master_metadata_album_artist_name
data_json$album  <- data_json$master_metadata_album_album_name





## Identify overlap timestamp
## First song of this is Perfect at 2018-01-17 05:18:12
data_lastfm %>%
    arrange(datetime_utc) %>%
    select(track, artist, album, datetime_utc) %>%
    head(5)

## With this setting we get a little bit of overlap
## Last song of this is Perfect at 2018-01-17 05:18:09 (slight time discrepancy)
## Second to last song is Waiting For Love at 2018-01-17 05:17:48
data_json %>%
    filter(datetime_utc <= as_datetime("2018-01-17 05:18:12")) %>%
    arrange(datetime_utc) %>%
    select(track, artist, album, datetime_utc) %>%
    tail(5)

## Last song of this is Waiting For Love at 2018-01-17 05:17:48
data_json %>%
    filter(datetime_utc <= as_datetime("2018-01-17 05:17:49")) %>%
    arrange(datetime_utc) %>%
    select(track, artist, album, datetime_utc) %>%
    tail(5)

## Filter to Waiting For Love
data_json_trimmed <- filter(data_json, datetime_utc <= as_datetime("2018-01-17 05:17:49"))

## Now filter out plays < 30s
data_json_filtered <- filter(data_json_trimmed, ms_played >= 30000)



## Final merge
music_data <- bind_rows(data_json_filtered, data_lastfm) %>%
    select(datetime_utc, track, artist, album) %>%
    mutate(
        year  = format(datetime_utc, "%Y", tz = "UTC"),
        month = format(datetime_utc, "%b %Y", tz = "UTC"),
        day   = format(datetime_utc, "%d %b %Y", tz = "UTC"),
        sec   = as.numeric(datetime_utc)
    )

write.csv(music_data, "output_data/260810_music_data.csv", row.names = FALSE)
## -----------------------------------------------------------------------------




## SUBSTITUTIONS
## -----------------------------------------------------------------------------
## Rules must be a named list, where each name is a column name in the df
## Values must be character vectors of length 1 or 2
## If length == 1, it will be used for subsetting (e.g. artist = "Coldplay")
## If length == 2, the second value will replace all instances of the first value 
##     in the given subset (e.g. track = c("Everglow - Edit", "Everglow"))
replace_plays <- function(df, rules) {
    ## Validate lengths
    for (col in names(rules)) {
        if (length(rules[[col]]) %notin% c(1, 2)) {
            stop(paste0("Length of rule for column ", col, " is illegal (", length(rules[[col]]), ")"))
        }
    }
    
    # 1. Build mask for matching rows
    mask <- rep(TRUE, nrow(df))
    for (col in names(rules)) {
        mask <- mask & (df[[col]] %in% rules[[col]][1])
    }
    
    # 2. Apply replacements for length-2 vectors
    for (col in names(rules)) {
        if (length(rules[[col]]) == 2) {
            df[[col]][mask] <- rules[[col]][2]
        }
    }
    
    return(df)
}


substitutions <- list(
    list(artist = "Coldplay", track = c("Everglow - Edit", "Everglow")),
    list(artist = )
)

## -----------------------------------------------------------------------------




track_grouped <- music_data %>%
    group_by(track, artist) %>%
    summarise(plays = n())

artist_grouped <- music_data %>%
    group_by(artist) %>%
    summarise(plays = n())

tracks_year <- music_data %>%
    filter(year == "2016") %>%
    group_by(track, artist) %>%
    summarise(plays = n())

albums_year <- music_data %>%
    filter(year == "2016") %>%
    group_by(album, artist) %>%
    summarise(plays = n())

artists_year <- music_data %>%
    filter(year == "2016") %>%
    group_by(artist) %>%
    summarise(plays = n())
