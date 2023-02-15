Shiny.addCustomMessageHandler("updateCountBar",
  function(message) {
    /* update bar values and size*/
    let bar1 = document.getElementById(message.id + "-count_bar_filtered");
    let bar2 = document.getElementById(message.id + "-count_bar_unfiltered");

    bar1.style.width = message.countnow / message.counttotal * 100 + "%";
    bar2.style.width = (message.countmax - message.countnow) / message.counttotal * 100 + "%";
  }
);

Shiny.addCustomMessageHandler("updateCountLabel",
  function(message) {
    /* updates Text */
    let e1 = document.getElementById(message.id + "-count_text");
    e1.textContent = message.label;
  }
);
