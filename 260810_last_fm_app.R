library(shiny)
library(shinyMobile)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cachem)
library(ggtext)
library(stringr)

shinyOptions(cache = cache_disk("./app_cache", max_age = 86400))

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
                    margin-top: 15px !important;
                    margin-bottom: 15px !important;
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
                        choices = c("Year" = "year", "Month" = "month", "Week" = "week", "Day" = "day", "Custom" = "custom"),
                        selected = "year"
                    ),
                    
                    # Shows for predefined intervals (Year/Month/Week/Day)
                    conditionalPanel(
                        condition = "input.date_mode != 'custom'",
                        f7Select(
                            "date_reference", 
                            "Reference date",
                            choices = c("To date", "Calendar"),
                            select = "To date",
                        )
                    )
                ),
    
                conditionalPanel(
                    condition = "input.date_mode != 'custom' & input.date_reference == 'Calendar'",
                    uiOutput("dynamic_date_choices")
                ),
                
                
                # Shows only when "Custom" is selected
                conditionalPanel(
                    condition = "input.date_mode == 'custom'",
                    uiOutput("custom_date_choices")
                )
            )
        ),
        
        f7Sheet(
            id = "sheet_settings",
            orientation = "bottom",
            swipeToClose = TRUE,
            backdrop = TRUE,
            f7BlockTitle("Plot Settings"),
            f7Block(
                f7Select(
                    "plot_color",
                    label = "Bar Color",
                    choices = c("Blue" = "#007aff", "Green" = "#4cd964", "Red" = "#ff3b30", "Purple" = "#af52de"),
                    selected = "#007aff"
                ),
                f7Stepper("plot_start", "Starting Rank (e.g., 1st)", min = 1, max = 100, value = 1),
                f7Stepper("plot_count", "How many bars to show", min = 5, max = 50, value = 10, step = 5)
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
            
            ## The 4 main tabs
            f7Tabs(
                id = "tabs",
                swipeable = TRUE,
                animated = FALSE,
                
                f7Tab(
                    title = "Artists",
                    tabName = "Artists",
                    icon = f7Icon("person_circle"),
                    f7Block("artists graph")
                ),
                
                f7Tab(
                    title = "Albums",
                    tabName = "Albums",
                    icon = f7Icon("music_albums"),
                    f7Block("albums graph")
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
                    tabName = "over_time",
                    icon = f7Icon("calendar"),
                    f7Block("time graph")
                ),
                
                f7Tab(
                    title = "Recents",
                    tabName = "Recents",
                    icon = f7Icon("music_note_list"),
                    f7Block("recents list")
                )
            )
        )
        ## ---------------------------------------------------------------------
    ),
    
    
    
    server = function(input, output, session) {
        ## Bind the top bar buttons to open their respective sheets
        observeEvent(input$btn_subset, { session$sendCustomMessage("open_f7_popup", "popup_subset") })
        observeEvent(input$btn_date, { session$sendCustomMessage("open_f7_popup", "popup_date") })
        observeEvent(input$btn_settings, { updateF7Sheet(id = "sheet_settings") })
        observeEvent(input$btn_input, { updateF7Sheet(id = "sheet_input") })
        
        
        ## Load the data
        raw_data <- reactive({
            req(input$input_csv)
            read.csv(input$input_csv) %>%
                mutate(across(
                    c(artist, album, track),
                    ~ str_to_title(.) %>% 
                        str_replace_all('"', "'")
                ))
        }) %>% bindCache(input$input_csv, Sys.Date())
        
        ## Change all dates when timezone changes
        full_data <- reactive({
            req(input$selected_timezone)
            raw_data() %>%
                select(datetime_utc, track, artist, album) %>%
                mutate(
                    datetime_utc = as_datetime(datetime_utc),
                    year    = format(datetime_utc, "%Y", tz = input$selected_timezone),
                    month   = format(datetime_utc, "%b %Y", tz = input$selected_timezone),
                    day     = format(datetime_utc, "%d %b %Y", tz = input$selected_timezone),
                    utc_sec = as.numeric(datetime_utc)
                )
        })
        
        
        # Dynamically generate the secondary dropdown for Dates based on interval
        date_picker_title_style <- "font-weight: bold; font-size: 16px; margin-bottom: -10px; margin-right: -10px, color: #8e8e93;"
        output$dynamic_date_choices <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            mode <- input$date_mode
            
            if (mode != "week") {
                years <- as.character(unique(df$year))
                months <- month.name
                days <- as.character(1:31)
                
                day_picker <- f7Picker(
                    "day_select", "Day", 
                    value = format(Sys.time(), "%d", tz = input$selected_timezone), 
                    choices = days, scrollToInput = TRUE
                )
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
                        
                        # If a picker is NULL, tags$div() creates an empty placeholder keeping the column structure intact
                        tags$div(day_picker),
                        tags$div(month_picker),
                        tags$div(year_picker)
                    )
                )
                
            } else {
                "weeks still need to be processed"
            }
        })
        
        output$custom_date_choices <- renderUI({
            df <- full_data()
            req(is.data.frame(df), nrow(df) > 0)
            
            start_datetime <- min(df$datetime_utc)
            years <- as.character(unique(df$year))
            months <- month.name
            days <- as.character(1:31)

            
            ## Set up pickers
            start_day_picker <- f7Picker(
                "custom_start_day", "Day", 
                value = format(start_datetime, "%d", tz = input$selected_timezone), 
                choices = days, scrollToInput = TRUE
            )
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
            
            end_day_picker <- f7Picker(
                "custom_end_day", "Day", 
                value = format(Sys.time(), "%d", tz = input$selected_timezone), 
                choices = days, scrollToInput = TRUE
            )
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
            
            
            ## Create two divs
            tags$div(
                tags$div(
                    tags$div("Start Date", style = date_picker_title_style),
                    tags$div(
                        class = "custom-stacked-pickers",
                        style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px; width: 100%;",
    
                        tags$div(start_day_picker),
                        tags$div(start_month_picker),
                        tags$div(start_year_picker)
                    )
                ),
                tags$div(
                    tags$div("End Date", style = date_picker_title_style),
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
        
        
        
        
        
        
        
        
        ## Update autocomplete options
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
            
            ## Subsetting date
            
            
            partial_subset
        })
        
        ## Read settings to get subset to graph
        graph_rows <- reactive({
            req(input$plot_start, input$plot_count)
            c(input$plot_start, input$plot_start + input$plot_count - 1)
        })
        
        ## Create graph
        output$tracks_graph <- renderPlot({
            req(input$tabs == "Tracks")
            
            ## Make these configurable later
            text_outside_threshold <- 0.15
            text_outside_displacement <- 0.25
            text_inside_colour <- "white"
            text_outside_colour <- "black"
            
            ##### could we instead do the first group_by dynamically, so that we only need this code once for tracks / albums / artists?
            tracks_data <- subset_data() %>%
                group_by(artist, track) %>%
                summarise(plays = n(), .groups = "drop") %>%
                arrange(desc(plays), track)
                
            max_plays <- ifelse(length(tracks_data$plays) > 0, max(tracks_data$plays), 0)
            
            tracks_data <- tracks_data %>%
                mutate(
                    rank = row_number(),
                    label = paste0(rank, "\\. **", track, "**<br>", artist),
                    is_short = plays < (max_plays * text_outside_threshold),
                    text_hjust = if_else(is_short, -text_outside_displacement, 1 + text_outside_displacement) 
                )
            
            req(nrow(tracks_data) > 0)
            
            idx <- graph_rows()
            max_row <- min(idx[2], nrow(tracks_data))
            plot_data <- tracks_data[idx[1]:max_row, ]
            
            ggplot(plot_data, aes(y = reorder(label, desc(rank)), x = plays)) +
                geom_col() +
                geom_text(aes(label = plays, hjust = text_hjust, col = as.character(is_short))) +
                scale_colour_manual(values = c("TRUE" = text_outside_colour, "FALSE" = text_inside_colour)) +
                coord_cartesian(xlim = c(0, NA), expand = FALSE) +
                theme_bw(base_size = 20) +
                theme(panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      axis.title = element_blank(),
                      axis.text.y = element_markdown()) +
                guides(col = "none")
        })
    }
)

app
