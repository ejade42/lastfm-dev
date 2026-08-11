library(shiny)
library(shinyMobile)
library(ggplot2)
library(dplyr)
library(lubridate)

app <- shinyApp(
    ui = f7Page(
        ## SHEETS FOR SETTINGS
        ## ---------------------------------------------------------------------
        f7Sheet(
            id = "sheet_subset",
            orientation = "bottom",
            swipeToClose = TRUE,
            backdrop = TRUE,
            f7BlockTitle("Filter Data Subset"),
            f7Block(
                f7Text("subset_artist", "Artist Name", placeholder = "e.g., Coldplay"),
                f7Text("subset_album", "Album (Optional)", placeholder = "e.g., Parachutes"),
                f7Text("subset_track", "Track (Optional)", placeholder = "e.g., Yellow")
            )
        ),
        
        f7Sheet(
            id = "sheet_date",
            orientation = "bottom",
            swipeToClose = TRUE,
            backdrop = TRUE,
            f7BlockTitle("Select Time Period"),
            f7Block(
                f7Select(
                    inputId = "date_mode",
                    label = "Interval Type",
                    choices = c("Year" = "year", "Month" = "month", "Week" = "week", "Day" = "day", "Custom" = "custom"),
                    selected = "year"
                ),
                
                # Shows for predefined intervals (Year/Month/Week/Day)
                conditionalPanel(
                    condition = "input.date_mode != 'custom'",
                    uiOutput("dynamic_date_choices")
                ),
                
                # Shows only when "Custom" is selected
                conditionalPanel(
                    condition = "input.date_mode == 'custom'",
                    dateInput("date_from", "From (Default: Start of dataset)", value = "2010-01-01"),
                    dateInput("date_to", "To (Default: Current)", value = Sys.Date())
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
                    inputId = "plot_color",
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
                        f7Button(
                            inputId = "btn_subset", 
                            label = "Subset", 
                            fill = FALSE, outline = FALSE, shadow = FALSE
                        ),
                        f7Button(
                            inputId = "btn_date", 
                            label = "Date", 
                            fill = FALSE, outline = FALSE, shadow = FALSE
                        ),
                        f7Button(
                            inputId = "btn_settings", 
                            label = "Settings", 
                            fill = FALSE, outline = FALSE, shadow = FALSE
                        ),
                        f7Button(
                            inputId = "btn_input",
                            label = "Input",
                            fill = FALSE, outline = FALSE, shadow = FALSE
                        )
                    )
                )
            ),
            
            ## The 4 main tabs
            f7Tabs(
                id = "tabs",
                swipeable = TRUE,
                animated = FALSE,
                
                f7Tab(
                    title = "Tracks",
                    tabName = "Tracks",
                    icon = f7Icon("music_note"),
                    f7Block(plotOutput("tracks_graph", height = "400px"))
                ),
                
                f7Tab(
                    title = "Albums",
                    tabName = "Albums",
                    icon = f7Icon("music_albums"),
                    f7Block("albums graph")
                ),
                
                f7Tab(
                    title = "Artists",
                    tabName = "Artists",
                    icon = f7Icon("person_circle"),
                    f7Block("artists graph")
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
        # Bind the top bar buttons to open their respective sheets
        observeEvent(input$btn_subset, { updateF7Sheet(id = "sheet_subset") })
        observeEvent(input$btn_date, { updateF7Sheet(id = "sheet_date") })
        observeEvent(input$btn_settings, { updateF7Sheet(id = "sheet_settings") })
        observeEvent(input$btn_input, { updateF7Sheet(id = "sheet_input") })
        
        
        # Dynamically generate the secondary dropdown for Dates based on interval
        output$dynamic_date_choices <- renderUI({
            mode <- input$date_mode
            
            # You can populate these dynamically based on your dataset later
            choices <- switch(mode,
                              "year" = c("Year to Date", "2026", "2025", "2024", "2023"),
                              "month" = c("Month to Date", "August 2026", "July 2026", "June 2026"),
                              "week" = c("Week to Date", "Last Week", "2 Weeks Ago"),
                              "day" = c("Today", "Yesterday", "2 Days Ago")
            )
            
            f7Select(
                inputId = "date_preset",
                label = paste("Select", tools::toTitleCase(mode)),
                choices = choices
            )
        })
        
        
        
        
        ## Load the data
        full_data <- reactive({
            req(input$input_csv)
            read.csv(input$input_csv)
        })
        
        subset_data <- reactive({
            partial_subset <- full_data()
            if (FALSE) {
            #if (!is.null(input$subset_artist)) {
                partial_subset <- filter(partial_subset, artist == input$subset_artist)
                    
                if (!is.null(input$subset_track)) {
                    partial_subset <- filter(partial_subset, track == input$subset_track)
                }
                if (!is.null(input$subset_album)) {
                    partial_subset <- filter(partial_subset, album == input$subset_album)
                }
            }
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
            
            tracks_data <- subset_data() %>%
                group_by(artist, track) %>%
                summarise(plays = n(), .groups = "drop") %>%
                arrange(desc(plays)) %>%
                mutate(
                    rank = row_number(),
                    label = paste0(rank, ". ", track, "\n", artist)
                )
            
            req(nrow(tracks_data) > 0)
            
            idx <- graph_rows()
            max_row <- min(idx[2], nrow(tracks_data))
            plot_data <- tracks_data[idx[1]:max_row, ]
            
            ggplot(plot_data, aes(y = reorder(label, plays), x = plays)) +
                geom_col() +
                scale_y_discrete(labels = )
        
        })
    }
)

app
