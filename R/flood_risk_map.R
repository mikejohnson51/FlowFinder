#' Make Flood Risk Max
#'
#' @param path a path the the maximum value
#'
#' @return a leaflet map
#' @export
#'

make_flood_risk_map = function(path = NULL){

vals = read_fst(path)
df = read_fst("data/comids_w_tz.fst")

#write_fst(df, path = "/Users/mikejohnson/Desktop/FlowlineMap/comids_w_tz.fst")

#save(df,file ="/Users/mikejohnson/Desktop/FlowlineMap/comids_w_tz.rda" )
#tz.shp = rgdal::readOGR("/Users/mikejohnson/Downloads/world/tz_world_mp.shp", stringsAsFactors = FALSE)
#sp = sp::SpatialPointsDataFrame(coords = cbind(df$long,df$lat), data = df, proj4string = AOI::HydroDataProj)
#tz.shp = tz.shp[sp,]

#tz = over(sp, tz.shp)

#df$tz = tz$TZID
#sp@data$tz = tz$TZID

#good = sp[which(!is.na(tz)), ]
#missing = sp[which(is.na(tz)), ]

#for( i in 1:nrow(missing@data)){
#missing@data$tz[i] = good$tz[ which.min(abs(missing@data$lat[i] - good@data$lat)) ]
#}

#df = rbind(missing@data, good@data)

#df = df[order(df$COMID),]

data = merge(vals, df, by = "COMID")

for(i in 1:nrow(data)){
data$locTime[i] = format(data$max_date[i], tz= data$tz[i],usetz=TRUE)
}

pop <- paste(
  paste("<strong>NHD COMID:</strong>", data$COMID),
  paste("<strong>Timezone:</strong>", data$tz),
  paste("<strong>Time of Peak (local):</strong>", data$locTime),
  paste("<strong>Time of Peak (UTC):</strong>", data$max_date),
  sep = "<br/>"
)

m = leaflet() %>% addProviderTiles(providers$CartoDB, group = "Base") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Terrain") %>%
  addCircleMarkers(
    data = data,
    lat = data$lat,
    lng = data$long,
    radius = (data$max) / 10000,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5,
    clusterOptions = markerClusterOptions(
      iconCreateFunction =
        JS(
          iconCreateFunction = JS(
            "function (cluster) {
            var childCount = cluster.getChildCount();
            if (childCount < 100) {
            c = 'rgba(255, 92, 93, .8);'
            } else if (childCount < 1000) {
            c = 'rgba(255, 0, 0, .8);'
            } else {
            c = 'rgba(128, 0, 0, .8);'
            }
            return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

            }")
        )),
    popup = pop
  ) %>%  addLayersControl(
    baseGroups = c("Base", "Imagery", "Terrain"),
    options = layersControlOptions(collapsed = T)
  )

return(m)

}

