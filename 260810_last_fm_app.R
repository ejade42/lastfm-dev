library(shiny)
library(shinyMobile)


app <- shinyApp(
    ui = f7Page(
        f7TabLayout(
            navbar = f7Navbar(
                title = "Combined Spotify/Last.fm viewer"
            ),
            f7Tabs(
                id = "tabs",
                swipeable = TRUE,
                animated = FALSE,
                
                f7Tab(
                    title = "Tracks",
                    tabName = "Tracks",
                    icon = f7Icon("music_note"),
                    f7Block("hi")
                ),
                
                f7Tab(
                    title = "Albums",
                    tabName = "Albums",
                    icon = f7Icon("music_albums"),
                    f7Block("hi")
                ),
                
                f7Tab(
                    title = "Artists",
                    tabName = "Artists",
                    icon = f7Icon("person_circle"),
                    f7Block("hi")
                ),
                
                f7Tab(
                    title = "Recents",
                    tabName = "Recents",
                    icon = f7Icon("music_note_list"),
                    f7Block("hi")
                )
            )
        )
    ),
    
    server = function(input, output, session) {
        
    }
)

app
