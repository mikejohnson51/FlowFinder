// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var zip = $el.data("zip");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    zip: zip,
    nonce: Math.random()
  });
});

// Click on flowline popup
$(document).on("click", ".stream_popup", function(e) {
  node = document.getElementsByClassName('leaflet-popup-content');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var comid = text.split("COMID:").pop();
  $($("#nav a")[2]).tab("show");
  Shiny.onInputChange("goto", {
    text : text,
    comid : comid
  });
});

// Click on flowline popup
$(document).on("click", "#mark_flowline", function(e) {
  $($("#nav a")[0]).tab("show");
});
 