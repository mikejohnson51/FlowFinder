// Get stream data from leaflet popup
function getStream(){
  text = document.getElementsByClassName('open-stream')[0].innerHTML;
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
  showTab(1);
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

$(document).on("click", "#slide", function(e) {
    console.log('toggle');
    $('#controls').toggleClass('off');
    var icon = $('#slide' + ' i');
    if (icon.hasClass('fa-caret-left')) {
      icon.addClass('fa-caret-right');
      icon.removeClass('fa-caret-left');
    } else {
      icon.addClass('fa-caret-left');
      icon.removeClass('fa-caret-right');
    }
});

$(document).on("click", ".go-stream", function(e) {
  e.preventDefault();
  $el = $(this);
  var stream = $el.data("stream");
  Shiny.onInputChange("switchStream", {
    stream: stream,
  });
});
 