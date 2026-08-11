library(shiny)
library(shinyMobile)


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
                
            )
        )
        ## ---------------------------------------------------------------------
        
        
        
        
        ## MAIN LAYOUT
        ## ---------------------------------------------------------------------
        f7TabLayout(
            navbar = f7Navbar(
                title = "Combined Spotify/Last.fm viewer",
                subNavbar = f7SubNavbar(
                    f7Grid(
                        cols = 3,
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
                    f7Block("tracks graph")
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
    }
)

app
