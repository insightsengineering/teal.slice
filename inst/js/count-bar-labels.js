Shiny.addCustomMessageHandler("updateCountBar",
  function(message) {
    /* update bar values and size*/
    let e2 = document.getElementById(message.id + "-count_bar");
    e2.setAttribute("aria-valuemin", message.countmin);
    e2.setAttribute("aria-valuemax", message.countmax);
    e2.setAttribute("aria-valuenow", message.countnow);
    e2.style.width = message.countnow / message.countmax * 100 + "%";
  }
);

Shiny.addCustomMessageHandler("updatecountLabel",
  function(message) {
    /* updates Text */
    let e1 = document.getElementById(message.id + "-count_text");
    e1.textContent = message.label;
  }
);