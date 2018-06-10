// Get stream data from leaflet popup
function getStream(node){
  node = document.getElementsByClassName('open-stream');
  var aNode = node[0];
  var text = aNode.innerHTML;
  var comid = text.split("COMID: ").pop();
  return [text, comid];
}

// Switch to a different tab
function showTab(num) { $($("#nav a")[num]).tab("show"); }

// close leaflet popup 
function closePopup(){ document.getElementsByClassName('leaflet-popup-close-button')[0].click(); }

// up and downstream buttons
$(document).on("click", ".upstream-flow", function(e) { stream("upStream"); });
$(document).on("click", ".downstream-flow", function(e) { stream("downStream"); });

// View on map button
$(document).on("click", "#mark_flowline", function(e) { showTab(0); });

// Reset button
$(document).on("click", "#reset", function(e) { $('#reset_buttons').removeClass('active'); });

// Graph button
$(document).on("click", ".stream-data", function(e) {
  showTab(2);
  closePopup();
  Shiny.onInputChange("goto", {
    text : getStream()[0]
  });
});

// up and downstream logic
function stream(direction) {
  Shiny.onInputChange(direction, {
    comid : ""
  });
  var elem = $('#reset_buttons');
  if(!elem.hasClass('active')){
    elem.addClass('active');
  }
  closePopup();
  Shiny.onInputChange(direction, {
    comid : getStream()[1]
  });
}
 