library(shiny)
library(shinyMobile)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cachem)
library(ggtext)
library(stringr)
library(jsonlite)
library(httr)
library(ggpattern)
library(purrr)
library(memoise)
library(shadowtext)
library(cowplot)
library(gridtext)

## option for printing lots of debugging statements
verbose <- TRUE
stepper_inf <- 1e9

shinyOptions(cache = cache_disk("./app_cache", max_age = 86400))
## do NOT end in a slash
image_location <- "app_cache/images"
dir.create(image_location, recursive = TRUE, showWarnings = FALSE)

lastfm_api_key <- readLines("api_lastfm.key")
spotify_client_id <- readLines("api_spotify.key")[1]
spotify_client_secret <- readLines("api_spotify.key")[2]

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

app <- shinyApp(
    ui = f7Page(
        tags$head(
            tags$script(HTML("
                Shiny.addCustomMessageHandler('open_f7_popup', function(id) {
                    app.popup.open('#' + id);
                });
            ")),
            tags$style(HTML("
                /* 1. Reduce the large outer margins of blocks and lists inside popups */
                .popup .block,
                .popup .list {
                    margin-top: 5px !important;
                    margin-bottom: 5px !important;
                }

                /* 2. Reduce the minimum height and padding of list items (like f7SmartSelect and f7Select) */
                .popup .list .item-content {
                    min-height: 36px !important; /* Default is usually 48px */
                }

                .popup .list .item-inner {
                    min-height: 36px !important;
                    padding-top: 4px !important;
                    padding-bottom: 4px !important;
                }

                #popup_subset .popup .list .item-content {
                    min-height: 48px !important;
                }

                #popup_subset .popup .list .item-inner {
                    min-height: 36px !important;
                    padding-top: 50px !important;
                    padding-bottom: 50px !important;
                }

                /* 3. Tighten the space between conditionalPanels and standalone inputs */
                .popup .shiny-input-container {
                    margin-bottom: 10px !important;
                }
            "))
        ),

        ## SHEETS FOR SETTINGS
        ## ---------------------------------------------------------------------
        customF7Popup(
            "popup_subset",
            "Filter Data Subset",
            f7List(
                uiOutput("ui_subset_artist"),
                uiOutput("ui_subset_album"),
                uiOutput("ui_subset_track")
            )
        ),

        customF7Popup(
            "popup_date",
            "Select Time Period",
            f7Block(
                apply_settings_button("btn_apply_date_filter", "Apply Date Filter"),

                f7SmartSelect(
                    "selected_timezone",
                    label = "Time zone",
                    choices = OlsonNames(),
                    openIn = "popup",
                    selected = Sys.timezone()
                ),

                f7Grid(cols = 2,
                    f7Select(
                        "date_mode",
                        "Interval",
                        choices = c("All time" = "alltime", "Year" = "year", "Month" = "month", "Week" = "week", "Day" = "day", "Custom" = "custom"),
                        selected = "alltime"
                    ),

                    # Shows for predefined intervals (Year/Month/Week/Day)
                    conditionalPanel(
                        condition = "input.date_mode != 'custom' & input.date_mode != 'alltime'",
                        f7Select(
                            "date_reference",
                            "Reference date",
                            choices = c("To today", "To date", "Calendar"),
                            select = "To today",
                        )
                    )
                ),

                # Shows only when 'Calendar' is selected for Year / Month / Day
                conditionalPanel(
                    condition = "input.date_mode != 'custom' & input.date_mode != 'alltime' & input.date_mode != 'week' & input.date_reference == 'Calendar'",
                    uiOutput("calendar_date_choices")
                ),

                # Shows only when 'Calendar' is selected for Week
                conditionalPanel(
                    condition = "input.date_mode == 'week' & input.date_reference == 'Calendar'",
                    uiOutput("calendar_date_choices_week")
                ),

                # Shows only when 'To date' is selected
                conditionalPanel(
                    condition = "input.date_mode != 'custom' & input.date_mode != 'alltime' & input.date_reference == 'To date'",
                    uiOutput("to_date_date_choices")
                ),

                # Shows only when "Custom" is selected
                conditionalPanel(
                    condition = "input.date_mode == 'custom'",
                    uiOutput("custom_date_choices")
                )
            )
        ),

        customF7Popup(
            "popup_settings",
            "Plot settings",
            f7Block(
                apply_settings_button("btn_apply_plot_settings", "Apply Plot Settings"),
                settings_row("Show title", f7Checkbox("plot_show_title", NULL, value = TRUE)),
                settings_row("Show dates", f7Checkbox("plot_show_dates", NULL, value = TRUE)),
                settings_row("Show subset", f7Checkbox("plot_show_subset", NULL, value = TRUE)),
                settings_row("Starting Rank (e.g., 1st)", f7Stepper("plot_start", NULL, min = 1, max = stepper_inf, value = 1, step = 1, manual = TRUE, decimalPoint = 0)),
                settings_row("How many bars to show", f7Stepper("plot_count", NULL, min = 5, max = 50, value = 10, step = 5, manual = TRUE, decimalPoint = 0)),
                settings_row("Thousands separator", f7Text("plot_thousands_sep", NULL, value = ",", placeholder = ",")),
                settings_row("Smart wrap target", f7Stepper("plot_smartwrap_target", NULL, min = 1, max = stepper_inf, value = 25, step = 5, manual = TRUE, decimalPoint = 0)),
                settings_row("Smart wrap max lines", f7Stepper("plot_smartwrap_max", NULL, min = 1, max = 10, value = 3, step = 1, manual = TRUE, decimalPoint = 0)),
                settings_row("Plot base size", f7Stepper("plot_base_size", NULL, min = 5, max = 50, value = 20, step = 1, manual = TRUE, decimalPoint = 0)),
                settings_row("Number text size", f7Stepper("plot_text_size", NULL, min = 0, max = 25, value = 10, step = 1, manual = TRUE, decimalPoint = 1)),
                settings_row("Bar outline colour", f7ColorPicker("plot_col_outline_colour", NULL, value = "#000000", modules = c("wheel", "hex"))),
                settings_row("Bar outline linewidth", f7Stepper("plot_col_linewidth", NULL, min = 0, max = 3, value = 1, step = 0.1, manual = TRUE, decimalPoint = 1)),
                settings_row("Threshold for text being outside", f7Stepper("plot_text_outside_threshold", NULL, min = 0, max = 1, value = 0.15, step = 0.05, manual = TRUE, decimalPoint = 2)),
                settings_row("Horizontal text displacement", f7Stepper("plot_text_displacement", NULL, min = 0, max = 1, value = 0.25, step = 0.05, manual = TRUE, decimalPoint = 2)),
                settings_row("Inside text colour", f7ColorPicker("plot_text_inside_colour", NULL, value = "#FFFFFF", modules = c("wheel", "hex"))),
                settings_row("Outside text colour", f7ColorPicker("plot_text_outside_colour", NULL, value = "#000000", modules = c("wheel", "hex"))),
                settings_row("Text shadow colour", f7ColorPicker("plot_text_shadow_colour", NULL, value = "#000000", modules = c("wheel", "hex"))),
                settings_row("Text shadow radius", f7Stepper("plot_text_shadow_radius", NULL, min = 0, max = 1, value = 0.1, step = 0.05, manual = TRUE, decimalPoint = 2)),
                settings_row("Outside text shadow alpha", f7Stepper("plot_text_outside_shadow_alpha", NULL, min = 0, max = 1, value = 0, step = 0.05, manual = TRUE, decimalPoint = 2)),
                h5("For 'Over time' plots only:"),
                settings_row("Bar colour", f7ColorPicker("plot_col_colour", NULL, value = "#FF0000", modules = c("wheel", "hex"))),
                settings_row("Max days to draw as hours",  f7Stepper("plot_max_days_to_draw_as_hours",  NULL, min = 1, max = stepper_inf, value = 1,   step = 1,  manual = TRUE, decimalPoint = 0)),
                settings_row("Max days to draw as days",   f7Stepper("plot_max_days_to_draw_as_days",   NULL, min = 1, max = stepper_inf, value = 40,  step = 5,  manual = TRUE, decimalPoint = 0)),
                settings_row("Max days to draw as months", f7Stepper("plot_max_days_to_draw_as_months", NULL, min = 1, max = stepper_inf, value = 370, step = 10, manual = TRUE, decimalPoint = 0)),
                settings_row("Hours display format",  f7Text("plot_hours_format",  NULL, value = "%H:00", placeholder = "%H:00")),
                settings_row("Days display format",   f7Text("plot_days_format",   NULL, value = "%a %d %b %Y", placeholder = "%a %d %b %Y")),
                settings_row("Months display format", f7Text("plot_months_format", NULL, value = "%b %Y", placeholder = "%b %Y")),
                settings_row("Years display format",  f7Text("plot_years_format",  NULL, value = "%Y", placeholder = "%Y")),
                h5("For 'Recents' plots only:"),
                settings_row("Timestamp display format",  f7Text("plot_timestamp_format",  NULL, value = "%a %d %b %Y, %H:%M:%S", placeholder = "%a %d %b %Y, %H:%M:%S")),
                settings_row("Timestamp text displacement", f7Stepper("plot_timestamp_displacement", NULL, min = 0, max = 1, value = 0.025, step = 0.005, manual = TRUE, decimalPoint = 3)),
            )
        ),

        f7Sheet(
            id = "sheet_input",
            orientation = "bottom",
            swipeToClose = TRUE,
            backdrop = TRUE,
            f7BlockTitle("Select Input"),
            f7Block(
                f7Text("input_csv", value = "https://raw.githubusercontent.com/ejade42/lastfm-dev/refs/heads/main/output_data/260810_music_data.csv")
            )
        ),
        ## ---------------------------------------------------------------------




        ## MAIN LAYOUT
        ## ---------------------------------------------------------------------
        f7TabLayout(
            navbar = f7Navbar(
                title = "Combined Spotify/Last.fm viewer",
                subNavbar = f7SubNavbar(
                    f7Grid(
                        cols = 4,
                        gap = 0,
                        f7Button("btn_subset", "Subset", fill = FALSE, outline = FALSE, shadow = FALSE),
                        f7Button("btn_date", "Date", fill = FALSE, outline = FALSE, shadow = FALSE),
                        f7Button("btn_settings", "Settings", fill = FALSE, outline = FALSE, shadow = FALSE),
                        f7Button("btn_input", "Input", fill = FALSE, outline = FALSE, shadow = FALSE)
                    )
                )
            ),

            ## The 5 main tabs
            f7Tabs(
                id = "tabs",
                swipeable = TRUE,
                animated = FALSE,

                f7Tab(
                    title = "Artists",
                    tabName = "Artists",
                    icon = f7Icon("person_circle"),
                    f7Block(plotOutput("artists_graph", width = "100%", height = "calc(100vh - 250px)"))
                ),

                f7Tab(
                    title = "Albums",
                    tabName = "Albums",
                    icon = f7Icon("music_albums"),
                    f7Block(plotOutput("albums_graph", width = "100%", height = "calc(100vh - 250px)"))
                ),

                f7Tab(
                    active = TRUE,
                    title = "Tracks",
                    tabName = "Tracks",
                    icon = f7Icon("music_note"),
                    f7Block(plotOutput("tracks_graph", width = "100%", height = "calc(100vh - 250px)"))
                ),

                f7Tab(
                    title = "Over time",
                    tabName = "Over_time",
                    icon = f7Icon("calendar"),
                    f7Block(plotOutput("over_time_graph", width = "100%", height = "calc(100vh - 250px)"))
                ),

                f7Tab(
                    title = "Recents",
                    tabName = "Recents",
                    icon = f7Icon("music_note_list"),
                    f7Block(plotOutput("recents_graph", width = "100%", height = "calc(100vh - 250px)"))
                )
            )
        )
        ## ---------------------------------------------------------------------
    ),



    server = function(input, output, session) {
        ## Bind the top bar buttons to open their respective sheets
        observeEvent(input$btn_subset, { session$sendCustomMessage("open_f7_popup", "popup_subset") })
        observeEvent(input$btn_date, { session$sendCustomMessage("open_f7_popup", "popup_date") })
        observeEvent(input$btn_settings, { session$sendCustomMessage("open_f7_popup", "popup_settings") })
        observeEvent(input$btn_input, { updateF7Sheet(id = "sheet_input") })

        ## DATA LOADING
        ## ---------------------------------------------------------------------
        ## Load the data
        raw_data <- reactive({
            if (verbose) {print("Raw data initialising", quote = F)}
            req(input$input_csv)
            if (verbose) {print("Raw data req passed", quote = F)}
            raw_data <- read.csv(input$input_csv) %>%
                mutate(across(
                    c(artist, album, track),
                    ~ str_to_title(.) %>%
                        str_replace_all('"', "'")
                ))
            if (verbose) {print("Raw data read", quote = F)}
            raw_data
        }) %>% bindCache(input$input_csv, Sys.Date())

        ## Change all dates when timezone changes
        full_data <- reactive({
            if (verbose) {print("Full data initialising", quote = F)}
            req(input$selected_timezone)
            if (verbose) {print("Full data req passed", quote = F)}
            full_data <- raw_data() %>%
                select(datetime_utc, track, artist, album) %>%
                mutate(
                    datetime_utc = as_datetime(datetime_utc),
                    date    = as_date(datetime_utc, tz = input$selected_timezone),
                    year    = format(datetime_utc, "%Y", tz = input$selected_timezone),
                    utc_sec = as.numeric(datetime_utc)
                )
            if (verbose) {print("Full data read, sorting", quote = F)}

            full_data <- full_data %>%
                arrange(desc(datetime_utc))

            if (verbose) {print("Full data sorted", quote = F)}
            full_data
        })
        ## ---------------------------------------------------------------------





        ## DATE SUBSET MANAGEMENT
        ## ---------------------------------------------------------------------
        date_picker_title_style <- "font-weight: bold; font-size: 16px; margin-bottom: -10px; margin-right: -10px, color: #8e8e93;"

        ## Create and populate main (non-custom) date picker
        output$calendar_date_choices <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            mode <- input$date_mode

            current_month   <- format(Sys.time(), "%B", tz = input$selected_timezone)
            current_year    <- format(Sys.time(), "%Y", tz = input$selected_timezone)
            current_day_num <- format(Sys.time(), "%d", tz = input$selected_timezone) %>% as.numeric()

            years <- as.character(sort(unique(df$year)))
            months <- month.name
            initial_day_choices <- generate_day_choices(current_year, current_month)


            day_picker <- uiOutput("calendar_day_picker_ui")
            month_picker <- f7Picker(
                "month_select", "Month",
                value = format(Sys.time(), "%B", tz = input$selected_timezone),
                choices = months, scrollToInput = TRUE
            )
            year_picker <- f7Picker(
                "year_select", "Year",
                value = format(Sys.time(), "%Y", tz = input$selected_timezone),
                choices = years, scrollToInput = TRUE
            )

            if (mode %in% c("month", "year")) {
                day_picker <- NULL
            }
            if (mode == "year") {
                month_picker <- NULL
            }

            tags$div(
                tags$div(paste0("Selected ", str_to_title(mode)), style = date_picker_title_style),
                tags$div(
                    class = "custom-stacked-pickers",
                    style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
                    tags$div(day_picker),
                    tags$div(month_picker),
                    tags$div(year_picker)
                )
            )
        })

        ## Create calendar week choices
        output$calendar_date_choices_week <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)

            week_from_datetime <- min(df$datetime_utc)
            week_from_month    <- format(week_from_datetime, "%B", tz = input$selected_timezone)
            week_from_year     <- format(week_from_datetime, "%Y", tz = input$selected_timezone)
            week_from_day_num  <- format(week_from_datetime, "%d", tz = input$selected_timezone) %>% as.numeric()

            week_to_datetime <- Sys.time()
            week_to_month    <- format(week_to_datetime, "%B", tz = input$selected_timezone)
            week_to_year     <- format(week_to_datetime, "%Y", tz = input$selected_timezone)
            week_to_day_num  <- format(week_to_datetime, "%d", tz = input$selected_timezone) %>% as.numeric()

            years <- as.character(sort(unique(df$year)))
            months <- month.name
            week_from_day_choices <- generate_day_choices(week_from_year, week_from_month)
            week_to_day_choices   <- generate_day_choices(week_to_year, week_to_month)

            week_from_day_picker <- uiOutput("week_from_day_picker_ui")
            week_from_month_picker <- f7Picker(
                "week_from_month", "Month",
                value = format(week_from_datetime, "%B", tz = input$selected_timezone),
                choices = months, scrollToInput = TRUE
            )
            week_from_year_picker <- f7Picker(
                "week_from_year", "Year",
                value = format(week_from_datetime, "%Y", tz = input$selected_timezone),
                choices = years, scrollToInput = TRUE
            )

            week_to_day_picker <- uiOutput("week_to_day_picker_ui")
            week_to_month_picker <- f7Picker(
                "week_to_month", "Month",
                value = format(Sys.time(), "%B", tz = input$selected_timezone),
                choices = months, scrollToInput = TRUE
            )
            week_to_year_picker <- f7Picker(
                "week_to_year", "Year",
                value = format(Sys.time(), "%Y", tz = input$selected_timezone),
                choices = years, scrollToInput = TRUE
            )

            tags$div(
                tags$div(
                    tags$div("Week from:", style = date_picker_title_style),
                    tags$div(
                        class = "custom-stacked-pickers",
                        style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
                        tags$div(week_from_day_picker),
                        tags$div(week_from_month_picker),
                        tags$div(week_from_year_picker)
                    )
                ),
                tags$div(
                    tags$div("Week to:", style = date_picker_title_style),
                    tags$div(
                        class = "custom-stacked-pickers",
                        style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
                        tags$div(week_to_day_picker),
                        tags$div(week_to_month_picker),
                        tags$div(week_to_year_picker)
                    ),
                    tags$div("Lol I can't get these linked so it's just custom date selection again")
                )
            )
        })

        ## Create to-date date picker
        output$to_date_date_choices <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            mode <- input$date_mode

            current_month   <- format(Sys.time(), "%B", tz = input$selected_timezone)
            current_year    <- format(Sys.time(), "%Y", tz = input$selected_timezone)
            current_day_num <- format(Sys.time(), "%d", tz = input$selected_timezone) %>% as.numeric()

            years <- as.character(sort(unique(df$year)))
            months <- month.name
            initial_day_choices <- generate_day_choices(current_year, current_month)

            to_date_day_picker <- uiOutput("to_date_day_picker_ui")
            to_date_month_picker <- f7Picker(
                "to_date_month_select", "Month",
                value = format(Sys.time(), "%B", tz = input$selected_timezone),
                choices = months, scrollToInput = TRUE
            )
            to_date_year_picker <- f7Picker(
                "to_date_year_select", "Year",
                value = format(Sys.time(), "%Y", tz = input$selected_timezone),
                choices = years, scrollToInput = TRUE
            )

            tags$div(
                tags$div(paste0(str_to_title(mode), " to:"), style = date_picker_title_style),
                tags$div(
                    class = "custom-stacked-pickers",
                    style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
                    tags$div(to_date_day_picker),
                    tags$div(to_date_month_picker),
                    tags$div(to_date_year_picker)
                )
            )
        })

        ## Create custom date picker
        output$custom_date_choices <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)

            start_datetime <- min(df$datetime_utc)
            start_month    <- format(start_datetime, "%B", tz = input$selected_timezone)
            start_year     <- format(start_datetime, "%Y", tz = input$selected_timezone)
            start_day_num  <- format(start_datetime, "%d", tz = input$selected_timezone) %>% as.numeric()

            end_datetime <- Sys.time()
            end_month    <- format(end_datetime, "%B", tz = input$selected_timezone)
            end_year     <- format(end_datetime, "%Y", tz = input$selected_timezone)
            end_day_num  <- format(end_datetime, "%d", tz = input$selected_timezone) %>% as.numeric()

            years <- as.character(sort(unique(df$year)))
            months <- month.name
            start_day_choices <- generate_day_choices(start_year, start_month)
            end_day_choices   <- generate_day_choices(end_year, end_month)

            start_day_picker <- uiOutput("custom_start_day_picker_ui")
            start_month_picker <- f7Picker(
                "custom_start_month", "Month",
                value = format(start_datetime, "%B", tz = input$selected_timezone),
                choices = months, scrollToInput = TRUE
            )
            start_year_picker <- f7Picker(
                "custom_start_year", "Year",
                value = format(start_datetime, "%Y", tz = input$selected_timezone),
                choices = years, scrollToInput = TRUE
            )

            end_day_picker <- uiOutput("custom_end_day_picker_ui")
            end_month_picker <- f7Picker(
                "custom_end_month", "Month",
                value = format(Sys.time(), "%B", tz = input$selected_timezone),
                choices = months, scrollToInput = TRUE
            )
            end_year_picker <- f7Picker(
                "custom_end_year", "Year",
                value = format(Sys.time(), "%Y", tz = input$selected_timezone),
                choices = years, scrollToInput = TRUE
            )

            tags$div(
                tags$div(
                    tags$div("Start date:", style = date_picker_title_style),
                    tags$div(
                        class = "custom-stacked-pickers",
                        style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
                        tags$div(start_day_picker),
                        tags$div(start_month_picker),
                        tags$div(start_year_picker)
                    )
                ),
                tags$div(
                    tags$div("End date:", style = date_picker_title_style),
                    tags$div(
                        class = "custom-stacked-pickers",
                        style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
                        tags$div(end_day_picker),
                        tags$div(end_month_picker),
                        tags$div(end_year_picker)
                    )
                )
            )
        })



        ## Update user choice ONLY when user physically changes the day picker
        ## 1. State tracking for values and skip counters
        saved_days <- reactiveValues(
            calendar = as.numeric(format(Sys.time(), "%d")),
            calendar_target = as.numeric(format(Sys.time(), "%d")),

            week_from = 1,
            week_from_target = 1,
            week_to = as.numeric(format(Sys.time(), "%d")),
            week_to_target = as.numeric(format(Sys.time(), "%d")),

            to_date = as.numeric(format(Sys.time(), "%d")),
            to_date_target = as.numeric(format(Sys.time(), "%d")),

            custom_start = 1,
            custom_start_target = 1,
            custom_end = as.numeric(format(Sys.time(), "%d")),
            custom_end_target = as.numeric(format(Sys.time(), "%d"))
        )

        ## Counter to absorb server-initiated UI re-render echoes
        skip_count <- reactiveValues(
            calendar = 1,
            week_from = 1,
            week_to = 1,
            to_date = 1,
            custom_start = 1,
            custom_end = 1
        )

        ## 2. Observers: Only update target if event was NOT produced by a server re-render
        observeEvent(input$day_select, {
            if (!is.null(input$day_select) && input$day_select != "" && input$day_select != "NA") {
                extracted <- as.numeric(stringr::str_extract(input$day_select, "\\d+"))
                if (!is.na(extracted)) {
                    if (skip_count$calendar > 0) {
                        skip_count$calendar <- skip_count$calendar - 1  # Swallow the echo
                    } else {
                        saved_days$calendar_target <- extracted       # True user scroll
                    }
                    saved_days$calendar <- extracted
                }
            }
        })

        observeEvent(input$week_from_day, {
            if (!is.null(input$week_from_day) && input$week_from_day != "" && input$week_from_day != "NA") {
                extracted <- as.numeric(stringr::str_extract(input$week_from_day, "\\d+"))
                if (!is.na(extracted)) {
                    if (skip_count$week_from > 0) {
                        skip_count$week_from <- skip_count$week_from - 1
                    } else {
                        saved_days$week_from_target <- extracted
                    }
                    saved_days$week_from <- extracted
                }
            }
        })

        observeEvent(input$week_to_day, {
            if (!is.null(input$week_to_day) && input$week_to_day != "" && input$week_to_day != "NA") {
                extracted <- as.numeric(stringr::str_extract(input$week_to_day, "\\d+"))
                if (!is.na(extracted)) {
                    if (skip_count$week_to > 0) {
                        skip_count$week_to <- skip_count$week_to - 1
                    } else {
                        saved_days$week_to_target <- extracted
                    }
                    saved_days$week_to <- extracted
                }
            }
        })

        observeEvent(input$to_date_day_select, {
            if (!is.null(input$to_date_day_select) && input$to_date_day_select != "" && input$to_date_day_select != "NA") {
                extracted <- as.numeric(stringr::str_extract(input$to_date_day_select, "\\d+"))
                if (!is.na(extracted)) {
                    if (skip_count$to_date > 0) {
                        skip_count$to_date <- skip_count$to_date - 1
                    } else {
                        saved_days$to_date_target <- extracted
                    }
                    saved_days$calendar <- extracted
                }
            }
        })

        observeEvent(input$custom_start_day, {
            if (!is.null(input$custom_start_day) && input$custom_start_day != "" && input$custom_start_day != "NA") {
                extracted <- as.numeric(stringr::str_extract(input$custom_start_day, "\\d+"))
                if (!is.na(extracted)) {
                    if (skip_count$custom_start > 0) {
                        skip_count$custom_start <- skip_count$custom_start - 1
                    } else {
                        saved_days$custom_start_target <- extracted
                    }
                    saved_days$custom_start <- extracted
                }
            }
        })

        observeEvent(input$custom_end_day, {
            if (!is.null(input$custom_end_day) && input$custom_end_day != "" && input$custom_end_day != "NA") {
                extracted <- as.numeric(stringr::str_extract(input$custom_end_day, "\\d+"))
                if (!is.na(extracted)) {
                    if (skip_count$custom_end > 0) {
                        skip_count$custom_end <- skip_count$custom_end - 1
                    } else {
                        saved_days$custom_end_target <- extracted
                    }
                    saved_days$custom_end <- extracted
                }
            }
        })

        ## 3. Render Blocks: Increment skip counter before rendering
        output$calendar_day_picker_ui <- renderUI({
            req(input$month_select, input$year_select)
            new_choices <- generate_day_choices(input$year_select, input$month_select)

            target_day <- isolate(saved_days$calendar_target)
            effective_day <- min(target_day, length(new_choices))

            # Queue 1 skip for the upcoming JS binding event
            isolate({ skip_count$calendar <- skip_count$calendar + 1 })

            f7Picker(
                "day_select", "Day",
                value = new_choices[effective_day],
                choices = new_choices, scrollToInput = TRUE
            )
        })

        output$week_from_day_picker_ui <- renderUI({
            req(input$week_from_month, input$week_from_year)
            new_choices <- generate_day_choices(input$week_from_year, input$week_from_month)

            target_day <- isolate(saved_days$week_from_target)
            effective_day <- min(target_day, length(new_choices))

            isolate({ skip_count$week_from <- skip_count$week_from + 1 })

            f7Picker(
                "week_from_day", "Day",
                value = new_choices[effective_day],
                choices = new_choices, scrollToInput = TRUE
            )
        })

        output$week_to_day_picker_ui <- renderUI({
            req(input$week_to_month, input$week_to_year)
            new_choices <- generate_day_choices(input$week_to_year, input$week_to_month)

            target_day <- isolate(saved_days$week_to_target)
            effective_day <- min(target_day, length(new_choices))

            isolate({ skip_count$week_to <- skip_count$week_to + 1 })

            f7Picker(
                "week_to_day", "Day",
                value = new_choices[effective_day],
                choices = new_choices, scrollToInput = TRUE
            )
        })

        output$to_date_day_picker_ui <- renderUI({
            req(input$to_date_month_select, input$to_date_year_select)
            new_choices <- generate_day_choices(input$to_date_year_select, input$to_date_month_select)

            target_day <- isolate(saved_days$to_date_target)
            effective_day <- min(target_day, length(new_choices))

            # Queue 1 skip for the upcoming JS binding event
            isolate({ skip_count$to_date <- skip_count$to_date + 1 })

            f7Picker(
                "to_date_day_select", "Day",
                value = new_choices[effective_day],
                choices = new_choices, scrollToInput = TRUE
            )
        })

        output$custom_start_day_picker_ui <- renderUI({
            req(input$custom_start_month, input$custom_start_year)
            new_choices <- generate_day_choices(input$custom_start_year, input$custom_start_month)

            target_day <- isolate(saved_days$custom_start_target)
            effective_day <- min(target_day, length(new_choices))

            isolate({ skip_count$custom_start <- skip_count$custom_start + 1 })

            f7Picker(
                "custom_start_day", "Day",
                value = new_choices[effective_day],
                choices = new_choices, scrollToInput = TRUE
            )
        })

        output$custom_end_day_picker_ui <- renderUI({
            req(input$custom_end_month, input$custom_end_year)
            new_choices <- generate_day_choices(input$custom_end_year, input$custom_end_month)

            target_day <- isolate(saved_days$custom_end_target)
            effective_day <- min(target_day, length(new_choices))

            isolate({ skip_count$custom_end <- skip_count$custom_end + 1 })

            f7Picker(
                "custom_end_day", "Day",
                value = new_choices[effective_day],
                choices = new_choices, scrollToInput = TRUE
            )
        })
        ## ---------------------------------------------------------------------




        ## POPULATE ARTIST / ALBUM / TRACK SUBSET INPUTS
        ## ---------------------------------------------------------------------
        output$ui_subset_artist <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            names(df) <- tolower(names(df))

            artist_options <- c("", sort(unique(df$artist)))

            f7SmartSelect("subset_artist", "Artist", choices = artist_options, openIn = "popup", searchbar = TRUE)
        })

        output$ui_subset_album <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            names(df) <- tolower(names(df))

            sel_artist <- input$subset_artist
            if (!is.null(sel_artist) && sel_artist != "") {
                df <- filter(df, tolower(artist) == tolower(sel_artist))
            }

            album_options <- c("", sort(unique(df$album)))
            f7SmartSelect("subset_album", "Album", choices = album_options, openIn = "popup", searchbar = TRUE)
        })

        output$ui_subset_track <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            names(df) <- tolower(names(df))

            sel_artist <- input$subset_artist
            sel_album  <- input$subset_album
            if (!is.null(sel_artist) && sel_artist != "") {
                df <- filter(df, tolower(artist) == tolower(sel_artist))
            }
            if (!is.null(sel_album) && sel_album != "") {
                df <- filter(df, tolower(album) == tolower(sel_album))
            }

            track_options <- c("", sort(unique(df$track)))
            f7SmartSelect("subset_track", "Track", choices = track_options, openIn = "popup", searchbar = TRUE)
        })
        ## ---------------------------------------------------------------------





        ## PERFORM ACTUAL DATA SUBSETTING
        ## ---------------------------------------------------------------------
        applied_date_range <- reactive({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)

            mode              <- input$date_mode %||% "alltime"
            selected_timezone <- input$selected_timezone %||% Sys.timezone()
            date_reference    <- input$date_reference %||% "To today"

            switch(
                input$date_mode,
                "alltime"={
                    c(
                        as_date(min(df$datetime_utc), tz = selected_timezone),
                        as_date(max(df$datetime_utc), tz = selected_timezone)
                    )
                },


                "year"={
                    if (date_reference == "Calendar") {
                        c(
                            as_date(paste0(input$year_select, "-01-01")),
                            as_date(paste0(input$year_select, "-12-31"))
                        )
                    } else if (date_reference == "To today") {
                        c(
                            as_date(Sys.time() - years(1) + days(1), tz = selected_timezone),
                            as_date(Sys.time(), tz = selected_timezone)
                        )
                    } else if (date_reference == "To date") {
                        sel_date <- extract_numeric_date(input$to_date_day_select, input$to_date_month_select, input$to_date_year_select)
                        c(
                            as_date(sel_date - years(1) + days(1), tz = selected_timezone),
                            sel_date
                        )
                    }
                },


                "month"={
                    if (date_reference == "Calendar") {
                        month_num <- sprintf("%02d", match(input$month_select, month.name))
                        first_of_month <- as_date(paste0(input$year_select, "-", month_num, "-01"))
                        days_this_month <- sprintf("%02d", days_in_month(first_of_month))
                        c(
                            first_of_month,
                            as_date(paste0(input$year_select, "-", month_num, "-", days_this_month))
                        )
                    } else if (date_reference == "To today") {
                        c(
                            as_date(Sys.time() - months(1) + days(1), tz = selected_timezone),
                            as_date(Sys.time(), tz = selected_timezone)
                        )
                    } else if (date_reference == "To date") {
                        sel_date <- extract_numeric_date(input$to_date_day_select, input$to_date_month_select, input$to_date_year_select)
                        c(
                            as_date(sel_date - months(1) + days(1), tz = selected_timezone),
                            sel_date
                        )
                    }
                },


                "week"={
                    if (date_reference == "Calendar") {
                        sel_week_from <- extract_numeric_date(input$week_from_day, input$week_from_month, input$week_from_year)
                        sel_week_to <- extract_numeric_date(input$week_to_day, input$week_to_month, input$week_to_year)
                        c(
                            sel_week_from,
                            sel_week_to
                        )
                    } else if (date_reference == "To today") {
                        c(
                            as_date(Sys.time() - days(6), tz = selected_timezone),
                            as_date(Sys.time(), tz = selected_timezone)
                        )
                    } else if (date_reference == "To date") {
                        sel_date <- extract_numeric_date(input$to_date_day_select, input$to_date_month_select, input$to_date_year_select)
                        c(
                            as_date(sel_date - days(6), tz = selected_timezone),
                            sel_date
                        )
                    }
                },


                "day"={
                    if (date_reference == "Calendar") {
                        sel_date <- extract_numeric_date(input$day_select, input$month_select, input$year_select)
                        c(
                            sel_date,
                            sel_date
                        )
                    } else if (date_reference == "To today") {
                        c(
                            as_date(Sys.time(), tz = selected_timezone),
                            as_date(Sys.time(), tz = selected_timezone)
                        )
                    } else if (date_reference == "To date") {
                        sel_date <- extract_numeric_date(input$to_date_day_select, input$to_date_month_select, input$to_date_year_select)
                        c(
                            sel_date,
                            sel_date
                        )
                    }
                },


                "custom"={
                    sel_start_date <- extract_numeric_date(input$custom_start_day, input$custom_start_month, input$custom_start_year)
                    sel_end_date <- extract_numeric_date(input$custom_end_day, input$custom_end_month, input$custom_end_year)
                    c(
                        sel_start_date,
                        sel_end_date
                    )
                }
            )
        }) %>% bindEvent(full_data(), input$btn_apply_date_filter, ignoreInit = FALSE)

        plot_settings <- reactive({
            list(
                show_title = input$plot_show_title,
                show_dates = input$plot_show_dates,
                show_subset = input$plot_show_subset,
                base_size = input$plot_base_size,
                text_size = input$plot_text_size,
                thousands_sep = input$plot_thousands_sep,
                smartwrap_target = input$plot_smartwrap_target,
                smartwrap_max = input$plot_smartwrap_max,
                col_outline_colour = input$plot_col_outline_colour$hex,
                col_linewidth = input$plot_col_linewidth,
                text_outside_threshold = input$plot_text_outside_threshold,
                text_displacement = input$plot_text_displacement,
                text_inside_colour = input$plot_text_inside_colour$hex,
                text_outside_colour = input$plot_text_outside_colour$hex,
                text_shadow_colour = input$plot_text_shadow_colour$hex,
                text_outside_shadow_alpha = input$plot_text_outside_shadow_alpha,
                text_shadow_radius = input$plot_text_shadow_radius,
                graph_rows = c(input$plot_start, input$plot_start + input$plot_count - 1),
                col_colour = input$plot_col_colour$hex,
                max_days_to_draw_as_hours = input$plot_max_days_to_draw_as_hours,
                max_days_to_draw_as_days = input$plot_max_days_to_draw_as_days,
                max_days_to_draw_as_months = input$plot_max_days_to_draw_as_months,
                hours_format = input$plot_hours_format,
                days_format = input$plot_days_format,
                months_format = input$plot_months_format,
                years_format = input$plot_years_format,
                timestamp_format = input$plot_timestamp_format,
                timestamp_displacement = input$plot_timestamp_displacement
            )
        }) %>% bindEvent(full_data(), input$btn_apply_plot_settings, ignoreInit = FALSE)

        subset_data <- reactive({
            partial_subset <- full_data()
            ## Subsetting artist / track / album
            if (isTruthy(input$subset_artist)) {
                partial_subset <- filter(partial_subset, tolower(artist) == tolower(input$subset_artist))
            }
            if (isTruthy(input$subset_album)) {
                partial_subset <- filter(partial_subset, tolower(album) == tolower(input$subset_album))
            }
            if (isTruthy(input$subset_track)) {
                partial_subset <- filter(partial_subset, tolower(track) == tolower(input$subset_track))
            }

            ## Subsetting date - Inclusive at both ends
            date_range <- applied_date_range()

            partial_subset <- filter(
                partial_subset,
                as_date(datetime_utc, tz = input$selected_timezone) >= as_date(min(date_range)),
                as_date(datetime_utc, tz = input$selected_timezone) <= as_date(max(date_range))
            )

            partial_subset
        })
        ## ---------------------------------------------------------------------





        ## GRAPHING
        ## ---------------------------------------------------------------------
        ## Reusable graph function
        generate_entity_plot <- function(data, entity, settings, date_range) {

            # 1. Dynamically select grouping columns
            group_cols <- if (entity == "artist") {
                "artist"
            } else if (entity == "album") {
                c("artist", "album")
            } else {
                c("artist", "track") # Default to track
            }

            # 2. Group and summarize
            entity_data <- data %>%
                group_by(across(all_of(group_cols))) %>%
                summarise(plays = n(), .groups = "drop") %>%
                arrange(desc(plays), .data[[entity]]) %>%
                mutate(rank = row_number())
            req(nrow(entity_data) > 0)

            # 3. Filter to desired graph rows
            idx <- settings$graph_rows
            max_row <- min(idx[2], nrow(entity_data))
            plot_data <- entity_data[idx[1]:max_row, ]

            max_plays <- ifelse(nrow(entity_data) > 0, max(plot_data$plays), 0)


            # 4. Create labels
            # 5. Dynamically fetch images based on the entity
            fallback_image <- "fallback_image.jpg"

            plot_data <- plot_data %>%
                mutate(
                    label = if (entity == "artist") {
                        paste0(rank, "\\. **", smart_wrap(artist, settings$smartwrap_target, settings$smartwrap_max), "**")
                    } else {
                        paste0(rank, "\\. **", smart_wrap(.data[[entity]], settings$smartwrap_target, settings$smartwrap_max),
                               "**<br>", smart_wrap(artist, settings$smartwrap_target, settings$smartwrap_max))
                    },
                    is_short = plays < (max_plays * settings$text_outside_threshold),
                    text_hjust = if_else(is_short, -settings$text_displacement, 1 + settings$text_displacement),

                    image_url = switch(
                        entity,
                        "artist" = map_chr(artist, ~ get_image(artist = .x, size = 4)),
                        "album"  = map2_chr(artist, album, ~ get_image(artist = .x, album = .y, size = 4)),
                        "track"  = map2_chr(artist, track, ~ get_image(artist = .x, track = .y, size = 4))
                    ),
                    image_url = if_else(is.na(image_url) | image_url == "", fallback_image, image_url)
                )

            if (verbose) {print(plot_data)}

            # 6. Generate titles
            left_title <- stringr::str_to_title(entity)
            right_title <- paste0(date_range[1], " to ", date_range[2])

            art_val <- ifelse(is.null(input$subset_artist) || input$subset_artist == "", "All", input$subset_artist)
            alb_val <- ifelse(is.null(input$subset_album) || input$subset_album == "", "All", input$subset_album)
            tra_val <- ifelse(is.null(input$subset_track) || input$subset_track == "", "All", input$subset_track)
            caption <- paste0("<b>Artist: </b>", art_val, "<br><b>Album: </b>", alb_val, "<br><b>Track: </b>", tra_val)

            if (!settings$show_title & !settings$show_dates) {left_title <- NULL}
            if (!settings$show_title & settings$show_dates) {left_title <- ""}
            if (!settings$show_dates) {right_title <- NULL}
            if (!settings$show_subset) {caption <- NULL}

            # 7. Generate the plot
            ggplot(plot_data, aes(y = reorder(label, desc(rank)), x = plays)) +
                geom_col_pattern(aes(pattern_filename = image_url), pattern = "image", pattern_type = "expand", col = settings$col_outline_colour, linewidth = settings$col_linewidth) +
                geom_shadowtext(aes(label = prettyNum(plays, big.mark = settings$thousands_sep), hjust = text_hjust, col = as.character(is_short), bg.colour = as.character(is_short)),
                                bg.r = settings$text_shadow_radius, size = settings$text_size) +
                scale_colour_manual(values = c("TRUE" = settings$text_outside_colour, "FALSE" = settings$text_inside_colour)) +
                scale_discrete_manual(aesthetics = "bg.colour", values = c("TRUE" = alpha(settings$text_shadow_colour, settings$text_outside_shadow_alpha), "FALSE" = settings$text_shadow_colour)) +
                scale_pattern_filename_identity() +
                coord_cartesian(xlim = c(0, NA), expand = FALSE, clip = "off") +
                labs(title = left_title, tag = right_title, caption = caption) +
                theme_classic(base_size = settings$base_size) +
                theme(plot.title = element_text(face = "bold"),
                      plot.caption = element_markdown(size = rel(1), hjust = 0),
                      plot.tag.position = c(1, 1),
                      plot.tag = element_text(size = rel(1.2), hjust = 1, vjust = 1),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      panel.border = element_blank(),
                      axis.title = element_blank(),
                      axis.text.y = element_markdown(),
                      axis.ticks = element_blank(),
                      axis.line = element_blank(),
                      axis.text.x = element_blank()) +
                guides(col = "none", bg.colour = "none")
        }


        ## The three actual outputs
        output$tracks_graph <- renderPlot({
            req(input$tabs == "Tracks")
            if (verbose) {print("Tab selected: Tracks")}
            generate_entity_plot(
                data = subset_data(),
                entity = "track",
                settings = plot_settings(),
                date_range = applied_date_range()
            )
        })

        output$albums_graph <- renderPlot({
            req(input$tabs == "Albums")
            if (verbose) {print("Tab selected: Albums")}
            generate_entity_plot(
                data = subset_data(),
                entity = "album",
                settings = plot_settings(),
                date_range = applied_date_range()
            )
        })

        output$artists_graph <- renderPlot({
            req(input$tabs == "Artists")
            if (verbose) {print("Tab selected: Artists")}
            generate_entity_plot(
                data = subset_data(),
                entity = "artist",
                settings = plot_settings(),
                date_range = applied_date_range()
            )
        })

        output$over_time_graph <- renderPlot({
            req(input$tabs == "Over_time")
            if (verbose) {print("Tab selected: Over time")}


            subset_data <- subset_data()
            settings <- plot_settings()

            selected_timezone <- input$selected_timezone %||% Sys.timezone()

            date_range <- applied_date_range()
            date_range_days <- lubridate::interval(date_range[1], date_range[2]) %/% days(1) + 1


            ## Generate the list of all X, and create a column of the desired granularity
            if (date_range_days <= settings$max_days_to_draw_as_hours) {
                all_timepoints <- format(seq(as_datetime(floor_date(date_range[1], "day"), tz = selected_timezone),
                                             as_datetime(ceiling_date(date_range[2], "day"), tz = selected_timezone) - seconds(1),
                                             by = "hour"), format = settings$hours_format)
                subset_data$timepoint <- format(as_datetime(subset_data$datetime_utc, tz = selected_timezone), format = settings$hours_format)

            } else if (date_range_days <= settings$max_days_to_draw_as_days) {
                all_timepoints <- format(seq(date_range[1], date_range[2], by = "day"), format = settings$days_format)
                subset_data$timepoint <- format(as_datetime(subset_data$datetime_utc, tz = selected_timezone), format = settings$days_format)

            } else if (date_range_days <= settings$max_days_to_draw_as_months) {
                all_timepoints <- format(seq(floor_date(date_range[1], "month"),
                                             floor_date(date_range[2], "month"),
                                             by = "month"), format = settings$months_format)
                subset_data$timepoint <- format(as_datetime(subset_data$datetime_utc, tz = selected_timezone), format = settings$months_format)

            } else {
                all_timepoints <- format(seq(floor_date(date_range[1], "year"),
                                             floor_date(date_range[2], "year"),
                                             by = "year"), format = settings$years_format)
                subset_data$timepoint <- format(as_datetime(subset_data$datetime_utc, tz = selected_timezone), format = settings$years_format)

            }


            ## Subset data based on timepoint
            grouped_data <- subset_data %>%
                group_by(timepoint) %>%
                summarise(plays = n(), .groups = "drop") %>%
                complete(timepoint = all_timepoints, fill = list(plays = 0)) %>%
                mutate(timepoint = factor(timepoint, levels = all_timepoints))



            grouped_data$max_plays <- ifelse(nrow(grouped_data) > 0, max(grouped_data$plays), 0)
            grouped_data <- grouped_data %>%
                mutate(
                    is_short = plays < (max_plays * settings$text_outside_threshold),
                    text_hjust = if_else(is_short, -settings$text_displacement, 1 + settings$text_displacement)
                )


            ## Titles
            left_title <- paste0("**Total:** ", prettyNum(sum(grouped_data$plays), big.mark = settings$thousands_sep),
                                 "<span style='color: transparent;'>M</span>",
                                 "**Average:** ", round(sum(grouped_data$plays) / date_range_days, digits = 1), "/day")
            right_title <- paste0(date_range[1], " to ", date_range[2])

            art_val <- ifelse(is.null(input$subset_artist) || input$subset_artist == "", "All", input$subset_artist)
            alb_val <- ifelse(is.null(input$subset_album) || input$subset_album == "", "All", input$subset_album)
            tra_val <- ifelse(is.null(input$subset_track) || input$subset_track == "", "All", input$subset_track)
            caption <- paste0("<b>Artist: </b>", art_val, "<br><b>Album: </b>", alb_val, "<br><b>Track: </b>", tra_val)

            title_element <- element_markdown()
            if (!settings$show_title & !settings$show_dates) {left_title <- NULL}
            if (!settings$show_title & settings$show_dates) {
                left_title <- ""
                title_element <- element_text()
            }
            if (!settings$show_dates) {right_title <- NULL}
            if (!settings$show_subset) {caption <- NULL}


            ## Plot
            ggplot(grouped_data, aes(y = timepoint, x = plays)) +
                geom_col(fill = settings$col_colour, col = settings$col_outline_colour, linewidth = settings$col_linewidth) +
                geom_shadowtext(aes(label = prettyNum(plays, big.mark = settings$thousands_sep), hjust = text_hjust, col = as.character(is_short), bg.colour = as.character(is_short)),
                                bg.r = settings$text_shadow_radius, size = settings$text_size) +
                scale_colour_manual(values = c("TRUE" = settings$text_outside_colour, "FALSE" = settings$text_inside_colour)) +
                scale_discrete_manual(aesthetics = "bg.colour", values = c("TRUE" = alpha(settings$text_shadow_colour, settings$text_outside_shadow_alpha), "FALSE" = settings$text_shadow_colour)) +
                scale_y_discrete(limits = rev) +
                coord_cartesian(xlim = c(0, NA), expand = FALSE, clip = "off") +
                labs(title = left_title, tag = right_title, caption = caption) +
                theme_classic(base_size = settings$base_size) +
                theme(plot.title = title_element,
                      plot.caption = element_markdown(size = rel(1), hjust = 0),
                      plot.tag.position = c(1, 1),
                      plot.tag = element_text(size = rel(1.2), hjust = 1, vjust = 1),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      panel.border = element_blank(),
                      axis.title = element_blank(),
                      axis.text.y = element_markdown(),
                      axis.ticks = element_blank(),
                      axis.line = element_blank(),
                      axis.text.x = element_blank()) +
                guides(col = "none", bg.colour = "none")
        })




        output$recents_graph <- renderPlot({
            req(input$tabs == "Recents")
            if (verbose) {print("Tab selected: Recents")}

            ## Subset data and make sure to put it in reverse time order
            subset_data <- subset_data() %>%
                mutate(rank = row_number())
            settings <- plot_settings()

            selected_timezone <- input$selected_timezone %||% Sys.timezone()

            date_range <- applied_date_range()

            idx <- settings$graph_rows
            if (verbose) {print(paste0("Graph rows: ", idx[1], "-", idx[2]), quote = FALSE)}
            if (verbose) {print(paste0("Input plot start: ", input$plot_start))}
            if (verbose) {print(paste0("Input plot count: ", input$plot_count))}
            max_row <- min(idx[2], nrow(subset_data))

            if (verbose) {print("Creating plot data", quote = FALSE)}
            plot_data <- subset_data[idx[1]:max_row, ]

            plot_data <- plot_data %>%
                mutate(
                    label = paste0(rank, "\\. **", smart_wrap(track, settings$smartwrap_target, settings$smartwrap_max),
                                   "**<br>", smart_wrap(artist, settings$smartwrap_target, settings$smartwrap_max)),
                    nice_timestamp = format(as_datetime(datetime_utc, tz = selected_timezone), format = settings$timestamp_format),
                    image_url = map2_chr(artist, track, ~ get_image(artist = .x, track = .y, size = 4))
                )

            if (verbose) {print(plot_data)}

            # 6. Generate titles
            left_title <- "Recents"
            right_title <- paste0(date_range[1], " to ", date_range[2])

            art_val <- ifelse(is.null(input$subset_artist) || input$subset_artist == "", "All", input$subset_artist)
            alb_val <- ifelse(is.null(input$subset_album) || input$subset_album == "", "All", input$subset_album)
            tra_val <- ifelse(is.null(input$subset_track) || input$subset_track == "", "All", input$subset_track)
            caption <- paste0("<b>Artist: </b>", art_val, "<br><b>Album: </b>", alb_val, "<br><b>Track: </b>", tra_val)

            if (!settings$show_title & !settings$show_dates) {left_title <- NULL}
            if (!settings$show_title & settings$show_dates) {left_title <- ""}
            if (!settings$show_dates) {right_title <- NULL}
            if (!settings$show_subset) {caption <- NULL}

            if (verbose) {print("Just before recents plot", quote = FALSE)}

            # 7. Generate the plot
            ggplot(plot_data, aes(y = reorder(label, desc(rank)), x = 1)) +
                geom_col_pattern(aes(pattern_filename = image_url), pattern = "image", pattern_type = "expand", col = settings$col_outline_colour, linewidth = settings$col_linewidth) +
                geom_shadowtext(aes(label = nice_timestamp), col = settings$text_inside_colour, bg.colour = settings$text_shadow_colour,
                                hjust = 1 + settings$timestamp_displacement, bg.r = settings$text_shadow_radius, size = settings$text_size) +
                scale_pattern_filename_identity() +
                coord_cartesian(xlim = c(0, NA), expand = FALSE, clip = "off") +
                labs(title = left_title, tag = right_title, caption = caption) +
                theme_classic(base_size = settings$base_size) +
                theme(plot.title = element_text(face = "bold"),
                      plot.caption = element_markdown(size = rel(1), hjust = 0),
                      plot.tag.position = c(1, 1),
                      plot.tag = element_text(size = rel(1.2), hjust = 1, vjust = 1),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      panel.border = element_blank(),
                      axis.title = element_blank(),
                      axis.text.y = element_markdown(),
                      axis.ticks = element_blank(),
                      axis.line = element_blank(),
                      axis.text.x = element_blank()) +
                guides(col = "none", bg.colour = "none")
        })
        ## ---------------------------------------------------------------------
    }
)

app

