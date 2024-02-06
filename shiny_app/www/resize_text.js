// resize_text.js
window.addEventListener('resize', function() {
  var plotContainer = document.getElementById('myplot');
  var plotWidth = plotContainer.clientWidth;

  // Adjust text size based on the plot width
  var newTextSize = Math.max(10, Math.min(0.02 * plotWidth, 20));

  // Apply the new text size to all text elements in the plot
  var textElements = document.querySelectorAll('.text');
  textElements.forEach(function(element) {
    element.style.fontSize = newTextSize + 'px';
  });
});
