


source("SpotifyHelpers.R")

artiesten <- read.csv("artiesten.csv", stringsAsFactors=FALSE)
test = readRDS("data/SongsSF2016.Rds")

tracksSOngFestival2016DF = tail(test, n=43L)

test = arrange(test, Artist, PopDate)
test$y = lag(test$ArtistFollowers)
test$FollowerDelta = (test$ArtistFollowers - test$y)
test$FollowerDelta[ test$PopDate == test$PopDate[1]] = NA

songpreviews = readRDS("data/songsIDS.Rds")
SongsSF2016Features = readRDS(file= "data/songsSF2016Features.Rds")

edges = readRDS(file="data/edges.Rds")
nodes = readRDS(file = "data/nodes.Rds")
nodes$label = as.character(nodes$label)
nodes$id= as.character(nodes$id)

nodes = left_join(nodes, tracksSOngFestival2016DF[,c(1,6)], by = c("id" = "Artist"))
nodes$title = paste(as.character(nodes$label), "<br> track popularity",nodes$popularity)
nodes$value = 100*as.numeric(nodes$popularity)
nodes$value[ is.na(nodes$value )] = 5

nodesImages = readRDS("data/nodeImages.Rds")

server = function(input, output, session) {
  
# fill selection
  observe({
    # Create a list of new options, where the name of the items is something
    # like 'option label x 1', and the values are 'option-x-1'.
    ttt = unique(test$Artist)
    s_options <- as.list(ttt)
    
    # Change values for input$inSelect
    updateSelectInput(session, "Artist", choices = s_options, selected = "Frans")
   
    
  }) 
  
  
  
##### Introduction TAB #################################################################################
  
  output$intro <- renderUI({
    list(
      img(SRC="linkedEuro.JPG", height = 340),
      
      hr(),
      h4("This little shiny dashboard shows the popularity of the 43 Eurovision 2016 song contest songs. The data is retrieved from Spotify. 
          Every hour I am calling the API to retrieve popularity and artists followers data of all the 43 songs"),
      h4("Audio features from the 43 songs are also retrieved and can be viewed on third tab."),
      h4("A network of related Eurovision Artists can be seen on the last tab. 
          The green nodes are the Eurovision 2016 participants 
          while the red nodes are their related artists. Note that edges from participant to another participant are present"),
      
      h3("Cheers, Longhow")

    )
    
  })
  
###### Score tab #####################################################################################
  
  output$CurrentPop =  renderDataTable({
    ### ophalen spotify data 
    #tracksSOngFestival2016DF = GetCurrentSF2016Data()
    
   
    tracksSOngFestival2016DF = left_join(tracksSOngFestival2016DF, artiesten)
    tracksSOngFestival2016DF$popularity = as.numeric(tracksSOngFestival2016DF$popularity)
    
    tracksSOngFestival2016DF = left_join(tracksSOngFestival2016DF, songpreviews, by=c("Artist"="Artist"))
    
    CT = str_sub(str_trim(tracksSOngFestival2016DF$Flag),1,2)
    tracksSOngFestival2016DF$Country = paste0(
      "<img src='flags/png/",
      str_trim(tracksSOngFestival2016DF$Flag),
      "'",
      "</img>",
      " ", CT
    )
    
    Audio = paste0(
      "<audio controls style='width:250px height: 10px;' src='", tracksSOngFestival2016DF$preview_url,
"'> </audio>"
)
      
    tracksSOngFestival2016DF$Audio = Audio
   
    tracksSOngFestival2016DF = arrange(tracksSOngFestival2016DF, desc(popularity))
    DT::datatable(escape = FALSE, rownames = FALSE, tracksSOngFestival2016DF[,c(15,16,1,3,6,8,9)], 
                  options = list(
                    pageLength = 15
                  ))
  }
  )
  
#########  over time tab ###########################################################################
  
  output$plotpop = renderPlotly({
    s = input$Artist
    ArtistData = dplyr::filter(test, Artist %in% s)
    if(input$Var == "1")
    {
      p =plot_ly(ArtistData, x= ~PopDate, y = ~popularity , type = "scatter", color = ~Artist )
    }
    if(input$Var == "2")
    {
      p =plot_ly(ArtistData, x = ~PopDate, y = ~ArtistFollowers , type = "scatter", color = ~Artist )
    }
    if(input$Var == "3")
    {
      p = plot_ly(ArtistData, x = ~PopDate, y = ~FollowerDelta , type = "scatter", color = ~Artist )
    }
    p
  })
  
##########  song feautures tab #######################################################################
  
  output$AudioFeat =  renderDataTable({
    
    SongsSF2016Features = left_join(SongsSF2016Features, songpreviews)
    Audio = paste0(
      "<audio controls style='width:220px;' src='", SongsSF2016Features$preview_url,
      "'> </audio>"
    )
    
    SongsSF2016Features$Audio = Audio
    DT::datatable(escape = FALSE, rownames = FALSE, SongsSF2016Features[,c(1,3,5,6,7,8,10,11,12,13,14,15,23)], 
                  options = list(
                    pageLength = 10
                  ))
  })
  
###########  network tab ##############################################################################
  
  output$ArtistLinks <- renderVisNetwork({
    
    visNetwork(nodes , edges) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics( stabilization = FALSE)
    
  })
  
  output$nodeinfo = renderUI({
   
    if(input$ArtistLinks_selected != "" ){
      linkje = nodesImages$image2[nodesImages$naam == input$ArtistLinks_selected]
      list(
        h4("Artist image and top tracks of ", input$ArtistLinks_selected),
        img(src=linkje,  height = 275, width = 450, align="center")
      )
    }
  })
  
  output$TopTracks =  renderDataTable({
    if(input$ArtistLinks_selected != "" ){
      artistID = nodesImages$artistID[nodesImages$naam == input$ArtistLinks_selected]
      TopSOngs = GetArtistsTopTracks(artistID)
      Audio = paste0(
        "<audio controls style='width:250px;' src='", TopSOngs$previewurl,
        "'> </audio>"
      )
      TopSOngs$Audio = Audio
      DT::datatable(escape = FALSE, rownames = FALSE, TopSOngs[,c(1,2,4)], 
                    options = list(
                      pageLength = 7, searching = FALSE
                    ))
    }
    })
  
  
##########  tab met zoekresultaten ###########################################
  
  
  ZoekEenArtiest = reactive({
    SearchArtists(URLencode(input$artistSearch), top = 100)
  })
  
  
  output$SearchResults =  renderDataTable({
    trs = ZoekEenArtiest()
    if (dim(trs)[1] > 0){
      trs$image = paste0(
        "<img src='",
        trs$imagelink,
        "'",
        "height='80' width='90' </img>"
      )
      QQ = DT::datatable(escape = FALSE, rownames = FALSE, trs[,c(1,3,4,6)], 
                      options = list(
                        pageLength = 7, searching = FALSE
                      ))
    }
    else{
      QQ= DT::datatable(data = data.frame())
    }
    
  })
  
  

  
  
  
}