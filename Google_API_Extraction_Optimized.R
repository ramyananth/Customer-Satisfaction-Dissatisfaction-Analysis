library('rgdal')
library('raster')
library('rgeos')
library('dismo')
library('foreign')
library('RgoogleMaps')
library('ggmap')
library('rgdal')
library('maps')
library('mapdata')
library('maptools')
library('geosphere')
library('httr')

type_rel<-paste("school","university","bakery","bar","cafe","food","meal_delivery","meal_takeaway","restaurant","airport","bicycle_store","bus_station","car_rental","parking",sep=",")
setwd("C:/Users/ramya.ananth/Desktop/National ZipCode Shape Files/")

bg<-readOGR(".","cb_2015_us_zcta510_500k")
plot(bg[bg$GEOID10==10512,])
bg_zip<-bg[bg$GEOID10==10512,]
resolution<-4/69

grid<-raster(extent(bg_zip))
res(grid)<-c(resolution,resolution)
proj4string(grid)<-proj4string(bg)
gridpolygon <- rasterToPolygons(grid)
dry.grid <- intersect(bg_zip, gridpolygon)
dry.grid_simple<-gSimplify(dry.grid,0.005)
plot(dry.grid)
plot(dry.grid_simple)

dry.grid_for<-fortify(dry.grid_simple)

matrix<-matrix(data=NA,ncol=2,nrow=length(unique(dry.grid_for$id)),byrow=FALSE)
dataframe<-data.frame(matrix)
names(dataframe)<-c("lat","long")

for (x in c(1:length(unique(dry.grid_for$id)))){
  dataframe$long[x]<-centroid(dry.grid_for[dry.grid_for$id==x,c(1,2)])[1]
  dataframe$lat[x]<-centroid(dry.grid_for[dry.grid_for$id==x,c(1,2)])[2]
}

grid_center<-rbind(dry.grid_for[,c(1:2)],dataframe)
grid_center<-unique(grid_center)
ncenter<-nrow(grid_center)

data_hyper<-matrix(data=NA,ncol=4,nrow=0,byrow=FALSE)
data_hyper<-as.data.frame(data_hyper)
names(data_hyper)<-c("Name","Type","Lat","Long")

data_place<-matrix(data=NA,ncol=1,nrow=0,byrow=FALSE)
data_place<-as.data.frame(data_place)
names(data_place)<-"Place_ID"

for (x in c(1:ncenter)){
  
  res<-GET(paste("https://maps.googleapis.com/maps/api/place/radarsearch/json?location=",grid_center$lat[x],",",grid_center$long[x],"&radius=3500&type=noquote(type_rel)&key=AIzaSyBL0lF364l-rz953QUXPM4LJXz7ew4t7wM",sep=""))
  jsonAnsw<-content(res,"text")
  myDataframe<- jsonlite::fromJSON(content(res,"text"))
  data_place<-rbind(data_place,as.data.frame(myDataframe$results$place_id))
}
  
data_place<-unique(data_place)
num_results<-nrow(data_place)

data<-matrix(data=NA,ncol=4,nrow=num_results,byrow=FALSE)
data<-as.data.frame(data)
names(data)<-c("Name","Type","Lat","Long")

for (x in c(1:num_results)){
  detail<-GET(paste("https://maps.googleapis.com/maps/api/place/details/json?placeid=",as.character(noquote(data_place[x,1])),"&key=AIzaSyBL0lF364l-rz953QUXPM4LJXz7ew4t7wM",sep=""))
  jsonAnsw<-content(detail,"text")
  myDetail<- jsonlite::fromJSON(content(detail,"text"))
  data[x,1]<-myDetail$result$name[1]
  data[x,2]<-myDetail$result$types[1]
  data[x,3]<-myDetail$result$geometry$location$lat[1]
  data[x,4]<-myDetail$result$geometry$location$lng[1]
}
  
data_hyper<-unique(data)