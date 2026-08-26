last_fm_ui <- f7Page(
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
)
