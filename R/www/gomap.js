// Click on flowline popup
$(document).on("click", ".stream-data", function(e) {
  node = document.getElementsByClassName('open-stream');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var comid = text.split("COMID:").pop();
  $($("#nav a")[2]).tab("show");
  /* Close popup */
  document.getElementsByClassName('leaflet-popup-close-button')[0].click();
  Shiny.onInputChange("goto", {
    text : text,
    comid : comid
  });
});

$(document).on("click", ".upstream-flow", function(e) {
  Shiny.onInputChange("upStream", {
    comid : ""
  });
  node = document.getElementsByClassName('leaflet-popup-content');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var st1 = text.split("COMID:").pop();
  var comid = st1.split("</a>")[0];
  comid = comid.trim();
  /* Make undo button active */
  var elem = $('#reset_buttons');
  if(!elem.hasClass('active')){
    elem.addClass('active');
  }
  /* Close popup */
  document.getElementsByClassName('leaflet-popup-close-button')[0].click();
  Shiny.onInputChange("upStream", {
    comid : comid
  });
});

$(document).on("click", ".downstream-flow", function(e) {
  Shiny.onInputChange("downStream", {
    comid : ""
  });
  node = document.getElementsByClassName('leaflet-popup-content');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var st1 = text.split("COMID:").pop();
  var comid = st1.split("</a>")[0];
  comid = comid.trim();
  /* Make undo button active */
  var elem = $('#reset_buttons');
  if(!elem.hasClass('active')){
    elem.addClass('active');
  }
  /* Close popup */
  document.getElementsByClassName('leaflet-popup-close-button')[0].click();
  Shiny.onInputChange("downStream", {
    comid : comid
  });
});

// Click on flowline popup
$(document).on("click", "#mark_flowline", function(e) {
  $($("#nav a")[0]).tab("show");
});

$(document).on("click", "#reset", function(e) {
  var elem = $('#reset_buttons');
  elem.removeClass('active');
});
 