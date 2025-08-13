Shiny.addCustomMessageHandler("updateCountBar", function (message) {
  let unfilteredBar = document.getElementById(message.id + "-count_bar_unfiltered");
  let filteredBar = document.getElementById(message.id + "-count_bar_filtered");
  let label = document.getElementById(message.id + "-count_bar_label");

  // Update filtered bar (current count)
  if (filteredBar) {
    filteredBar.setAttribute("width", (message.countnow / message.counttotal * 100).toFixed(2) + "%");
  }
  
  // Update unfiltered bar (remaining count)
  if (unfilteredBar) {
    unfilteredBar.setAttribute("x", (message.countnow / message.counttotal * 100).toFixed(2) + "%");
    unfilteredBar.setAttribute("width", ((message.countmax - message.countnow) / message.counttotal * 100).toFixed(2) + "%");
  }
  
  if (label) {
    label.textContent = message.label;
  }
});

Shiny.addCustomMessageHandler("updateCountText", function (message) {
  let el = document.getElementById(message.id);
  el.textContent = message.label;
});
