Shiny.addCustomMessageHandler("updateCountBar",
  function(message) {
    let bar1 = document.getElementById(message.id + "-count_bar_filtered");
    let bar2 = document.getElementById(message.id + "-count_bar_unfiltered");
    
    bar1.style.width = message.countnow / message.counttotal * 100 + "%";
    bar2.style.width = (message.countmax - message.countnow) / message.counttotal * 100 + "%";
    bar1.textContent = message.label;
  }
);

Shiny.addCustomMessageHandler("updateCountText",
  function(message) {
    let el = document.getElementById(message.id)
    el.textContent = message.label
  }
);
