$(document).on('click', '#zoom_in', function() {
  var currentZoom = $('body').css('zoom');
  $('body').css('zoom', parseFloat(currentZoom) + 0.1);
});

$(document).on('click', '#zoom_out', function() {
  var currentZoom = $('body').css('zoom');
  $('body').css('zoom', parseFloat(currentZoom) - 0.1);
});
