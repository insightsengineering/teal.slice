function initFilterPanel() {
let filterHeaders = Array.from(document.getElementsByClassName("filter-card-title"))
// TO DO:
// - allow insert/removal of cards (though may do this with shiny)
// - listen on parent element to handle adding/removing cards 
//    (see last section of https://blog.webdevsimplified.com/2022-01/event-listeners/)
// - styling (use bootstrap vars so it will update with theme)
filterHeaders.forEach((el) => {
    el.addEventListener("click", (e) => {
        e.preventDefault()
        // if clicked card is active, we need to set inactive but
        //  _not_ reset it to active
        let clickedCardIsActive = e.target.classList.contains("active")
                // reset active card to inactive
        let activeHeaders = filterHeaders.filter((el) => el.classList.contains("active"))

        if (activeHeaders.length > 0) {
            let activeHeader = activeHeaders[0]
            let activeBody = activeHeader.parentElement.nextElementSibling;
            let activeIcon = activeHeader.querySelector(".filter-card-toggle-icon")

            activeHeader.classList.remove("active")
            activeBody.style.display = "none"
            activeIcon.classList.remove("fa-chevron-down")
            activeIcon.classList.add("fa-chevron-right")
            // animation
            activeBody.style.maxHeight = null;
       }

                // set new card active, _if_ new card was not the card clicked
        if (!clickedCardIsActive) {
                    //debugger
            let cardHeader = e.target
            let cardBody = cardHeader.parentElement.nextElementSibling;
            let cardIcon = cardHeader.querySelector(".filter-card-toggle-icon")

            cardHeader.classList.add("active")
            cardBody.style.display = "block"
            cardIcon.classList.remove("fa-chevron-right")
            cardIcon.classList.add("fa-chevron-down")

                    // animation
            if (cardBody.style.maxHeight) {
                cardBody.style.maxHeight = null;
            } else {
                cardBody.style.maxHeight = cardBody.scrollHeight + "px";
            }
        }
    })
})
};