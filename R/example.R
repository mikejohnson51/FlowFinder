
## Define a clip_unit
xxx  =  list ( 48.9142, -104.7749, 5,5)

## Get NHD, USGS, and NWM data
nhd = findNHD(clip_unit = xxx , ids = T)
usgs = findUSGS(clip_unit = xxx)


## Define USGS icon
usgsIcon = makeIcon(
  iconUrl= "https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",
  iconWidth = 40, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 10)

## Define New SL
sl = nhd$flowlines[nhd$flowlines$comid == nhd$ids[1],]

## Set Map
m = leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addPolylines(data = nhd$flowlines,
               color = 'blue',
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

  addPolylines(data = sl,
               color = 'red',
               weight = 6) %>%  ## ADD SINGLE LINE SHP !!!

  addScaleBar("bottomleft")

m


## Get HUC8 digits in AOI

huc8 = unique(substring(nhd$flowlines$reachcode,1,8))

## Ploting the data

plot( x = data[data$comid == nhd$ids[1],]$dateTime,
      y = data[data$comid == nhd$ids[1],]$cms,
     type = "l",
     col = 'blue',
     lwd =3,
     main = paste0("Streamflow (cfs)\nCOMID: ", nhd$id[1] ),
     ylab = "streamflow (cfs)",
     xlab = 'Date and Time')




##### USING NOMADS_SUBSET

source("./subset_nomads_rda.R")
nhd = HydroData::findNHD(clip_unit = list("UCSB", 10, 10), ids = TRUE)
df = subset_nomads_rda(comids = nhd$ids)


### NHD upstream

#nhd = findNHD(clip_unit = list("Colorado Springs", 10, 10))
nhd_prep = prep_nhd(flines = nhd$flowlines)

up_stream = get_upstream(flines = nhd_prep)

xxx = "17595277"

##upstream

plot(nhd$flowlines[nhd$flowlines$comid %in% c(xxx, up_stream[up_stream$comid == xxx, 2]),], col = "blue")

plot(nhd$flowlines[nhd$flowlines$comid %in% c(xxx, nhd_prep[nhd_prep$comid == xxx, 4]),], col = "green", add = T)


plot(nhd$flowlines[nhd$flowline$comid == xxx,], col = 'red', add = T)
