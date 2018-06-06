
## Define a clip_unit
xxx  =  list ( 48.9142, -104.7749, 5,5)

## Get NHD, USGS, and NWM data
nhd = findNHD(clip_unit = xxx , ids = T)
usgs = findUSGS(clip_unit = xxx)

## This works for now bt well need to figure out how to read from an external directory
## Well also need to find a way to automatically update the folder. It looks like a cron job linked to Google Drive might work...

#data = get_nomads(dir = "/Users/mikejohnson", comids = nhd$ids)
data = subset_nomads(comids = nhd$ids)

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
test = prep_nhd(flines =nhd$flowlines)
hmm = get_upstream(flines = test)
xxx = "1529819"
sum(nhd$flowlines$comid %in% c(xxx, hmm[hmm$comid == xxx,2]))
plot(nhd$flowlines[nhd$flowlines$comid %in% c(xxx, hmm[hmm$comid == xxx, 2]),], col = "blue")
plot(nhd$flowlines[nhd$flowline$comid == xxx,], col = 'red', add = T)
