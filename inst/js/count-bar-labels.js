Shiny.addCustomMessageHandler("updateCountBar", function (message) {
  let bar1 = document
    .getElementById(message.id + "-count_bar_filtered")
    .querySelector(".state-count-bar-filtered");
  let bar2 = document.getElementById(message.id + "-count_bar_unfiltered");

  bar1.style.width = (message.countnow / message.counttotal) * 100 + "%";
  bar2.style.width =
    ((message.countmax - message.countnow) / message.counttotal) * 100 + "%";
  document
    .getElementById(message.id + "-count_bar_filtered")
    .querySelector(".asdf").textContent = message.label;
});

Shiny.addCustomMessageHandler("updateCountText", function (message) {
  let el = document.getElementById(message.id);
  el.textContent = message.label;
});
