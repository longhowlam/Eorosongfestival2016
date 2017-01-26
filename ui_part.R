ui <- dashboardPage(
  dashboardHeader(title = "The Shiny Eurovision 2016 App", titleWidth = "330px"),
  dashboardSidebar(width = 200,
    
    sidebarMenu(
      
      ###### menuitems
      menuItem("Introduction", tabName = "Introduction", icon = icon("euro")),
      menuItem("Song popularity ranking", tabName = "SongRankings1", icon = icon("trophy")),
      menuItem("Over time evolution", tabName = "SongRankings2", icon = icon("trophy")),
      menuItem("Song/audio features", tabName = "StaticSong", icon = icon("music")),
      menuItem("Artists network", tabName = "ArtistNetwork", icon = icon("link")),
      menuItem("Search Artists", tabName = "ArtistNetwork2", icon = icon("link"))
      
      
    ) 
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Introduction",
        titlePanel("Introduction"),
          mainPanel(
            htmlOutput("intro")
          )
        ),
    
      tabItem(tabName = "SongRankings1",
        titlePanel("Song ranking based on spotify popluarity"),
          mainPanel(
              div(DT::dataTableOutput("CurrentPop"), style = "font-size:90%")
          )
        ),
      
      tabItem(tabName = "SongRankings2",
              titlePanel("Song ranking over time"),
              mainPanel(
                fluidRow(
                  column(8, selectInput(width = "100%", "Artist", "Select Artis(s)", multiple = TRUE, choices = list())),
                  column(4, selectInput("Var", "Select Variable", choices=list("Song Popularity" = 1, "Artist Followers" = 2, "Artist Delta Followers"=3)))
                ),
                plotlyOutput("plotpop", height = "650px")
                
              )
      ),
      tabItem(tabName = "StaticSong",
              titlePanel("Song/audio feautures from Spotify"),
              h4("More information on the meanning of audio feautures can be found!", a("on Spotify", href="https://developer.spotify.com/web-api/get-audio-features/")),
              mainPanel(
                div(DT::dataTableOutput("AudioFeat"), style = "font-size:85%")
              )
      ),
      tabItem(tabName = "ArtistNetwork",
              titlePanel("Network of Eurovision 2016 related artists"),
              mainPanel(
                fluidRow(
                  column(7,
                     h3("Zoomable, clickable and drag-able interactieve network graph"),
                     visNetworkOutput("ArtistLinks", height = "600px")
                  ),
                  column(5, 
                    h3("Information of selected node"),
                    htmlOutput("nodeinfo"),
                    div(DT::dataTableOutput( "TopTracks", width = "700px"), style = "font-size:90%")
                  )
                )
              )
      ),
      tabItem(tabName = "ArtistNetwork2",
              titlePanel("Search Artists"),
              mainPanel(
                fluidRow(
                  textInput("artistSearch", "Search key artist","Madonna"),
                  h4("search results artist"),
                  div(DT::dataTableOutput( "SearchResults", width = "500px"), style = "font-size:90%")
                
                )
              )
      )
      
    )
  )
)
