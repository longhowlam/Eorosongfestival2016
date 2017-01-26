library(httr)

GetSpotifyToken = function(clientID, secret){
  
  response <- POST('https://accounts.spotify.com/api/token',
                   accept_json(),
                   authenticate(clientID, secret),
                   body=list(grant_type='client_credentials'),
                   encode='form',
                   verbose())
  
  if (status_code(response) == 200){
    return(content(response)$access_token)
  }
  else{
    return("")
  }
}


ConvertTracksToDataFrame = function(tracks)
{
  naampjes = function(x)
  {
    c(x$track$artists[[1]]$name,
      x$track$id,
      x$track$name,
      x$track$artists[[1]]$id,
      x$track$duration_ms,
      x$track$popularity)
  }
  
  tmp = as.data.frame(stringsAsFactors  = FALSE, t(sapply( tracks, naampjes)))
  names(tmp) = c( "Artist","trackid", "Song","artistid" ,"duration", "popularity")
  tmp
}


ConvertPlayListsToDataFrame = function(PL)
{
  naampjes = function(x)
  {
    c(x$name  ,x$tracks$href   )
  }
  
  tmp = as.data.frame(stringsAsFactors  = FALSE, t(sapply( PL, naampjes)))
  names(tmp) = c("Name","Trackshref")
  tmp
}


GetAudioFeautures = function(trackid){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/audio-features/", trackid)
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  as.data.frame(stringsAsFactors  = FALSE, content(r2))
}


GetPreviewURL = function(trackid){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/tracks/", trackid)
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  tr = content(r2)
  tr$preview_url
}




#artistID = songIDS$artistid[i]
GetRelatedArtists = function(artistID){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/artists/", artistID, "/related-artists")
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  tr = content(r2)
  
  naampjes = function(x)
  {
    c(x$name,
      x$id,
      x$popularity)
  }
  if(length(tr[[1]]) > 0){
    tmp = as.data.frame(stringsAsFactors  = FALSE, t(sapply( tr[[1]], naampjes)))
    names(tmp) = c( "to", "artistid" ,"popularity")
    return(tmp)
  }
  else
  {
    return(data.frame())
  }
}  


GetRelatedArtists2 = function(artistID, num){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/artists/", artistID, "/related-artists")
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  tr = content(r2)
  
  naampjes = function(x)
  {
    if (length(x$images) > 0){
      return( c(x$name,
                x$id,
                x$popularity,
                x$images[[1]]$url)
      )
    }else{
      return( c(x$name,
                x$id,
                x$popularity,
                "")
      )
    }
    
  }
  if(length(tr[[1]]) > 0){
    tmp = as.data.frame(stringsAsFactors  = FALSE, t(sapply( tr[[1]], naampjes)))
    names(tmp) = c( "to", "artistid" ,"popularity","image")
    return(tmp[1:num,])
  }
  else
  {
    return(data.frame())
  }
}  


#artistID = songIDS$artistid[i]
GetArtistsINFO = function(artistID){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/artists/", artistID)
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  tr = content(r2)
  naam = tr$name
  if(length(tr$images) > 0){
    image1 = tr$images[[1]]$url
    image2 = tr$images[[2]]$url
  }else{
    image1 = image2 = ""
  }
  
  data.frame(naam, artistID, image1, image2)
}  





GetArtistsTopTracks = function(artistID){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/artists/", artistID, "/top-tracks?country=NL")
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  trs = content(r2)[[1]]
  Ntrs = length(trs)
  pop = prv = track = character(Ntrs)
  for (i in 1:Ntrs)
  {
    prv[i] = trs[[i]]$preview_url
    track[i] = trs[[i]]$name
    pop[i] = trs[[i]]$popularity
  }
  data.frame(track = track, popularity = pop, previewurl = prv)
}  


SearchArtists = function(artist, top = 5){
  
  clientID = "____YOURCLIENTID____"
  secret = "____YOURSECRET______"
  
  ### token opohalen
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  
  URI = paste0("https://api.spotify.com/v1/search?q=", artist, "&type=artist")
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  trs = content(r2)
  artiesten = trs[[1]]$items
  if( length(artiesten) > 0){
    
    N = min(top,trs[[1]]$limit)
    
    naam = id = imagelink =  character(N)
    followers = popularity = numeric(N)
    for (i in 1:N )
    {
      naam[i] = artiesten[[i]]$name
      id[i] = artiesten[[i]]$id
      followers[i] = as.numeric(artiesten[[i]]$followers$total)
      popularity[i] = as.numeric(artiesten[[i]]$popularity)
      if(length(artiesten[[i]]$images) > 0)
      {
        imagelink[i] = artiesten[[i]]$images[[1]]$url
      }
    }
    return(data.frame(naam, id , followers, popularity, imagelink, stringsAsFactors = FALSE))
  }else{
    return(data.frame())
  }
  
  
}  
