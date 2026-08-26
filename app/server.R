last_fm_server <- function(input, output, session) {
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
