filterCardListener = function (e) {

    let target = e.target;
    // e.currentTarget takes into account bubbling
    // i.e. it's the element the listener is actually attached to
    let container = e.currentTarget
    let containerId = container.id

    if (target.classList.contains('filter-card-remove')) {

        let id = target.id;
        Shiny.setInputValue(id, 1, { priority: 'event' })

        let numCardsInContainer = getHeaders(containerId).length

        if (numCardsInContainer === 0) {
            container.removeEventListener('click', filterCardListener)
        }
    }

    let header = target.closest('.filter-card-header')
    // if header is null, body was clicked (e.g. an input)
    if (header !== null) {
        toggleFilterCard(header, containerId)
    }
}


// there are 2 cases for the clicked card:
// 1. it is currently inactive
// 2. it is currently active

// if 1., we need to set the active card, which was not clicked, to inactive
//  this is handled in the first 'if' block

// if 2., we need to set the active card, which was clicked, to inactive
//  this is handled in the second 'if' block
toggleFilterCard = function (el, containerId) {

    let activeHeaders = getHeaders(containerId)
                        .filter((el) => el.classList.contains("active"))
    let clickedCardIsActive = el.classList.contains("active")

    if (activeHeaders.length > 0) {

        let activeHeader = activeHeaders[0]
        let activeBody = activeHeader.nextElementSibling;
        let activeIcon = activeHeader.querySelector(".filter-card-toggle")

        activeHeader.classList.remove("active")
        activeBody.style.display = "none"
        activeIcon.classList.remove("fa-chevron-down")
        activeIcon.classList.add("fa-chevron-right")
        activeBody.style.maxHeight = null; //animation

    }
    
    if (!clickedCardIsActive) {

        let cardHeader = el
        let cardBody = cardHeader.nextElementSibling;
        let cardIcon = cardHeader.querySelector(".filter-card-toggle")

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
}

getHeaders = function (containerId) {
    return Array.from(
        document
        .getElementById(containerId)
        .getElementsByClassName('filter-card-header')
    )
}


Shiny.addCustomMessageHandler('filter-card-add', function (id) {
    // since we're adding a listener by name, it will only get added once
    document.getElementById(id)
        .addEventListener('click', filterCardListener)
})

/*
Shiny.addCustomMessageHandler('filter-card-remove', function (id) {
    document.getElementById(id)
        .removeEventListener('click', filterCardListener)
})
*/