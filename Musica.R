## Prep

id<- 'a5fce28d7a6c48fdabb06905f215ce2a'
secret<- '8addcba8c65643d5823ac674d20b52a3'

Sys.setenv(SPOTIFY_CLIENT_ID = 'a5fce28d7a6c48fdabb06905f215ce2a' )
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8addcba8c65643d5823ac674d20b52a3')

library(Rspotify)
library(plyr)
library(tidyverse)
library(ggthemes)
library(ggridges)
library(knitr)
library(kableExtra)
library(lubridate)


rhye<- get_artist_audio_features('spotify:artist:2AcUPzkVWo81vumdzeLLRN')



## Prep 
install.packages("dplyr")
library(generics)
library(dplyr)
library(Rspotify)
library(httr)
library(jsonlite)
library(plyr)
library(glmnet)


## Endpoint
spotifyEndpoint<- oauth_endpoint(NULL,"https://accounts.spotify.com/authorize", "https://accounts.spotify.com/api/token" )

## Credenciales
key = spotifyOAuth("12173816375",client_id="a5fce28d7a6c48fdabb06905f215ce2a",client_secret="8addcba8c65643d5823ac674d20b52a3")

## Función para obtener el id de discos
getAlbums_id=function(id,type="album",market="US",token){
  require(dplyr)
  total<- httr::content(httr::GET(paste0("https://api.spotify.com/v1/artists/",id,"/albums?album_type=", type),
                                  httr::config(token = token)))$total
  req<-httr::GET(paste0("https://api.spotify.com/v1/artists/",id,
                        "/albums?offset=0&limit=",total,
                        "&album_type=",type,"&market=",market),
                 httr::config(token = token))
  return(bind_rows(lapply(httr::content(req)$items,
                          function(x) data.frame(
                            id = x$id,
                            stringsAsFactors = F))))
}

## Prueba para Rhye
getAlbums_id('2AcUPzkVWo81vumdzeLLRN',token = key)


## Función para obtener características de canciones

getFeatures_up<-function(spotify_ID,token){
  
  req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/",spotify_ID), httr::config(token = token))
  json1<-httr::content(req)
  dados=data.frame(id=as.character(json1$id),
                   danceability=as.numeric(json1$danceability),
                   energy=as.numeric(json1$energy),
                   key=as.numeric(json1$key),
                   loudness=as.numeric(json1$loudness),
                   mode=as.numeric(json1$mode),
                   speechiness=as.numeric(json1$speechiness),
                   acousticness=as.numeric(json1$acousticness),
                   instrumentalness=as.numeric(json1$instrumentalness),
                   liveness=as.numeric(json1$liveness),
                   valence=as.numeric(json1$valence),
                   tempo=as.numeric(json1$tempo),
                   stringsAsFactors = T)
  d=unlist(dados[1,], use.names=T)
  return(d)
}

## Prueba para canción Open de Rhye
open<-getFeatures_up('3JsA2sWDNR9oQogGAzqqtH',token=key)
open<- as.data.frame(open_1)
open

## Función para obtener las canciones de un albúm
getAlbum<-function(id,token){
  req<-httr::GET(paste0("https://api.spotify.com/v1/albums/",id,"/tracks"),httr::config(token = token))
  json1<-httr::content(req)
  json2<-jsonlite::fromJSON(jsonlite::toJSON(json1))$items
  json3=data.frame(id=unlist(json2$id),
                   name=unlist(json2$name))
  return(json3)
}

## Prueba para albúm Spirit de Rhye
getAlbum('4emDc6rcjqJGqCj8NqEIzG',token = key)


## Función para obtener información de cada canción de un albúm 

songs_albums_features=function(a){ 
  # input is the album_id
  #output all the stats of songs within a specific album
  
  album_info=data.frame(getAlbumInfo(a,key))
  album_artist=album_info$artist
  album_date=album_info$release_date
  #all the stats of songs in specific album 
  albums_frame=data.frame(getAlbum(a,key))  #this is the source of the problem
  tracks_ids=albums_frame$id
  tracks_matrix=matrix(0,nrow=length(tracks_ids),ncol=14)
  for(track in 1:length(tracks_ids)){
    tracks_matrix[track,]=c(getFeatures_up(tracks_ids[track],key),
                            as.character(album_date),
                            as.character(album_artist)
    )
  }
  tracks_all_info=data.frame(tracks_matrix)
  colnames(tracks_all_info)=c("id","danceability","energy","key","loudness",
                              "mode","speechiness","acousticness",
                              "instrumentalness", "liveness","valence",
                              "tempo","release_date","artist_name")
  tracks_all_info$track_name=albums_frame$name
  return(tracks_all_info)
  # returned a data-frame
}
combine_all_songs=function(ids){
  stats=list()
  for (i in 1:length(ids)){
    stats[i]=songs_albums_features(ids[i],key)
  }
}
get_all_works=function(artist_id){
  all_work=rbind.fill(lapply(getAlbums_id(artist_id,token=key)[,1],songs_albums_features)
  )
  all_work$id=as.character(all_work$id)
  all_work$danceability=as.numeric(all_work$danceability)
  all_work$energy=as.numeric(all_work$energy)
  all_work$key=as.numeric(all_work$key)
  all_work$loudness=as.numeric(all_work$loudness)
  all_work$mode=as.numeric(all_work$mode)
  all_work$speechiness=as.numeric(all_work$speechiness)
  all_work$acousticness=as.numeric(all_work$acousticness)
  all_work$instrumentalness=as.numeric(all_work$instrumentalness)
  all_work$liveness=as.numeric(all_work$liveness)
  all_work$valence=as.numeric(all_work$valence)
  all_work$tempo=as.numeric(all_work$tempo)
  all_work$release_date=as.character(all_work$release_date)
  all_work$artist_name=as.character(all_work$artist_name)
  all_work$track_name=as.character(all_work$track_name)
  return(all_work)
}

#Prueba para Rhye

inforhye<-get_all_works('432R46LaYsJZV2Gmc4jUV5')
View(inforhye)

########################### Visualizaciones #################3

library(ggplot2)
rhye=get_all_works('2AcUPzkVWo81vumdzeLLRN')
joy=get_all_works('432R46LaYsJZV2Gmc4jUV5')
View(joy)
View(rhye)
class(rhye)

comp=rbind(rhye,joy)

ggplot(comp, aes(x=loudness, fill=artist_name)) +
  geom_density(alpha=0.4) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

ggplot(rhye,aes(x=danceability))+
  geom_density()

ggplot(joy,aes(x=energy))+
  geom_density()

####################################################### Estadísticas de escuchados #############################

## Función para obtener las canciones de un albúm
getAlbum<-function(id,token){
  req<-httr::GET(paste0("https://api.spotify.com/v1/albums/",id,"/tracks"),httr::config(token = token))
  json1<-httr::content(req)
  json2<-jsonlite::fromJSON(jsonlite::toJSON(json1))$items
  json3=data.frame(id=unlist(json2$id),
                   name=unlist(json2$name))
  return(json3)
}


###


get_artist_top_tracks <- function(id,token) {
  req1<- httr::GET(paste0('https://api.spotify.com/v1/artists/',id,"/top-tracks"),httr::config(token = token))
  json11<- httr::content(req1)
  json22<- jsonlite::fromJSON(jsonlite::toJSON(json11))$items
  json33=data.frame(id=unlist(json22$id),
                    name=unlist(json22$name))
  return(json33)

}
get_artist_top_tracks('',token = key)




























