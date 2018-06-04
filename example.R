
## Define a clip_unit
xxx  =  list ( 38.9142, -104.7749, 5,5)

## Get NHD, USGS, and NWM data
nhd = findNHD(clip_unit = xxx , ids = T)
usgs = findUSGS(clip_unit = xxx)

## This works for now bt well need to figure out how to read from an external directory
## Well also need to find a way to automatically update the folder. It looks like a cron job linked to Google Drive might work...
data = get_nomads(dir = "/Users/mikejohnson", comids = nhd$ids)

## Define USGS icon
usgsIcon = makeIcon(
  iconUrl= "https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",
  iconWidth = 40, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 10)

## Set Map
m = leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addPolylines(data = nhd$flowlines,
               color = ifelse(nhd$flowlines$comid == nhd$ids[1], 'red','blue'),
               weight = nhd$flowlines$streamorde,
               label = paste0(paste0(nhd$flowlines$gnis_name),
                              paste0(" COMID:", nhd$flowlines$comid)),
               group = "NHD Flowlines") %>%

  addMarkers(data = usgs$nwis,
             icon = usgsIcon,
             group = "USGS Stations"
  )%>%

  addLayersControl(
    baseGroups = c("CartoDB","Imagery", "OSM"),
    overlayGroups = c("USGS Stations", "NHD Flowlines"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%

  addScaleBar("bottomleft")


