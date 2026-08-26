get_spotify_token <- function(client_id, client_secret) {
    auth_response <- POST(
        url = "https://accounts.spotify.com/api/token",
        body = list(grant_type = "client_credentials", client_id = client_id, client_secret = client_secret),
        encode = "form"
    )
    if (status_code(auth_response) != 200) stop("Authentication failed.")
    return(content(auth_response)$access_token)
}
get_spotify_token_cached <- memoise(get_spotify_token, cache = cache_mem(max_age = 3400))

## Function for getting url
get_image <- function(artist, album = NULL, track = NULL, size = 4) {
    image <- NULL

    file_prefix <- str_replace_all(tolower(artist), "[^a-z0-9]", "_")
    if (!is.null(album)) {
        file_suffix <- str_replace_all(tolower(album), "[^a-z0-9]", "_")
        type <- "album"
    } else if (!is.null(track)) {
        file_suffix <- str_replace_all(tolower(track), "[^a-z0-9]", "_")
        type <- "track"
    } else {
        file_suffix <- "artist"
        type <- "artist"
    }

    local_filename <- paste0(image_location, "/", type, "_", file_prefix, "_", file_suffix, ".jpg")
    if (file.exists(local_filename)) {
        return(local_filename)
    }

    if (!is.null(album)) {
        album_res <- httr::GET(
            url = "http://ws.audioscrobbler.com/2.0/",
            query = list(
                method  = "album.getInfo",
                api_key = lastfm_api_key,
                artist  = artist,
                album   = album,
                format  = "json"
            )
        )

        album_json <- httr::content(album_res)
        image <- album_json[["album"]][["image"]][[size]][["#text"]]

    } else if (!is.null(track)) {
        track_res <- httr::GET(
            url = "http://ws.audioscrobbler.com/2.0/",
            query = list(
                method  = "track.getInfo",
                api_key = lastfm_api_key,
                artist  = artist,
                track   = track,
                format  = "json"
            )
        )

        track_json <- httr::content(track_res)
        image <- track_json[["track"]][["album"]][["image"]][[size]][["#text"]]
    }

    if ((is.null(album) & is.null(track)) | is.null(image)) {
        ## Get artist image from spotify
        access_token <- get_spotify_token_cached(spotify_client_id, spotify_client_secret)

        search_response <- content(GET(
            url = "https://api.spotify.com/v1/search",
            add_headers(Authorization = paste("Bearer", access_token)),
            query = list(q = artist, type = "artist", limit = 1)
        ))

        image <- search_response[["artists"]][["items"]][[1]][["images"]][[1]][["url"]]
    }

    if (!is.null(image) && image != "") {
        tryCatch({
            download.file(url = image, destfile = local_filename, mode = "wb", quiet = TRUE)
            return(local_filename)
        }, error = function(e) {
            return(NA) # Return NA if download fails
        })
    }

    return(NA)
}

## Function for putting persistent popups into UI
customF7Popup <- function(id, title, ..., close_text = "Close") {
    shiny::tags$div(
        id = id,
        class = "popup",
        shiny::tags$div(class = "view",
                        shiny::tags$div(class = "page",
                                        # Popup Header / Navbar
                                        shiny::tags$div(class = "navbar",
                                                        shiny::tags$div(class = "navbar-bg"),
                                                        shiny::tags$div(class = "navbar-inner",
                                                                        shiny::tags$div(class = "title", title),
                                                                        shiny::tags$div(class = "right",
                                                                                        shiny::tags$a(href = "#", class = "link popup-close", close_text)
                                                                        )
                                                        )
                                        ),
                                        # Popup Content (uses the ... argument to insert your UI elements)
                                        shiny::tags$div(class = "page-content",
                                                        ...
                                        )
                        )
        )
    )
}

## Function for nice appearance of settings rows
settings_row <- function(title, control) {
    div(
        class = "settings-row",
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; border-bottom: 1px solid #333333; padding-bottom: 10px;",
        div(style = "flex: 1; text-align: left; padding-right: 15px; font-weight: 500;", title),
        div(style = "flex-shrink: 0;", control)
    )
}

apply_settings_button <- function(id, name) {
    div(
        style = "margin-top: -50px; margin-bottom: 15px;",
        f7Button(id, name, fill = TRUE),
    )
}

generate_day_choices <- function(year, month_name) {
    month_num <- match(month_name, month.name)
    first_day <- as.Date(paste(year, month_num, "01", sep = "-"))

    num_days <- lubridate::days_in_month(first_day)
    dates <- first_day + lubridate::days(0:(num_days - 1))

    day_nums <- as.character(1:num_days)
    weekdays <- lubridate::wday(dates, label = TRUE, abbr = TRUE)
    labels <- paste0("", weekdays, " ", day_nums)

    return(labels)
}

extract_numeric_date <- function(day_label, month_name, year) {
    month_num <- sprintf("%02d", match(month_name, month.name))
    day_num <- sprintf("%02d", as.numeric(stringr::str_extract(day_label, "\\d+")))
    constructed_date <- paste0(year, "-", month_num, "-", day_num)
    lubridate::as_date(constructed_date)
}


## Function for splitting text onto multiple lines in the most aesthetic way - RSS based
smart_wrap <- function(text, target = 20, max_lines = 3, br = "<br>") {
    sapply(text, function(x) {
        if (is.na(x) || nchar(x) == 0) return(x)

        words <- unlist(strsplit(x, "\\s+"))
        num_words <- length(words)
        total_len <- nchar(x)

        # Return untouched if within target length or single word
        if (total_len <= target || num_words <= 1) return(x)

        # Calculate ideal line count n
        n <- min(ceiling(total_len / target), max_lines, num_words)
        if (n <= 1) return(x)

        # Generate all valid partitions of words across n lines
        possible_splits <- combn(1:(num_words - 1), n - 1, simplify = FALSE)

        best_lines <- NULL
        best_score <- Inf
        ideal_len <- total_len / n

        for (split in possible_splits) {
            line_starts <- c(1, split + 1)
            line_ends   <- c(split, num_words)

            lines <- sapply(seq_along(line_starts), function(i) {
                paste(words[line_starts[i]:line_ends[i]], collapse = " ")
            })

            # Score metric: Sum of squared deviations from mean target line length
            lengths <- nchar(lines)
            score <- sum((lengths - ideal_len)^2)

            if (score < best_score) {
                best_score <- score
                best_lines <- lines
            }
        }

        paste(best_lines, collapse = br)
    }, USE.NAMES = FALSE)
}
