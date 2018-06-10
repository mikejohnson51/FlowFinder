// Get COMID from leaflet-popup
function getCOMID(node){
  node = document.getElementsByClassName('leaflet-popup-content');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var st1 = text.split("COMID:").pop();
  var comid = st1.split("</a>")[0];
  comid = comid.trim();
  return comid;
}

// Switch to a different tab
function showTab(num) {
  $($("#nav a")[num]).tab("show");
}

// Click on graph button
$(document).on("click", ".stream-data", function(e) {
  node = document.getElementsByClassName('open-stream');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var comid = text.split("COMID:").pop();
  showTab(2);
  document.getElementsByClassName('leaflet-popup-close-button')[0].click();
  Shiny.onInputChange("goto", {
    text : text,
    comid : comid
  });
});

// Determine what happens when up/downstream button clicked
function stream(direction) {
  Shiny.onInputChange(direction, {
    comid : ""
  });
  comid = getCOMID();
  var elem = $('#reset_buttons');
  if(!elem.hasClass('active')){
    elem.addClass('active');
  }
  document.getElementsByClassName('leaflet-popup-close-button')[0].click();
  Shiny.onInputChange(direction, {
    comid : comid
  });
}


$(document).on("click", ".upstream-flow", function(e) { stream("upStream"); });
$(document).on("click", ".downstream-flow", function(e) { stream("downStream"); });


// View on map button
$(document).on("click", "#mark_flowline", function(e) {
  showTab(0);
});

// Reset button
$(document).on("click", "#reset", function(e) {
  $('#reset_buttons').removeClass('active');
});
 