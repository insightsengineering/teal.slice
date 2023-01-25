
// all filter card accordian actions are based on card headers
// we listen for clicks on headers and dispatch based on what element
//  was clicked (e.g. remove icon)
// we also use card headers as an 'anchor' point from which we locate
//  other elements of the card (e.g. card body)

// containerId is the Shiny namespaced id if the filter card container
//  for a particular variable (e.g. AGE's filter card container)
getHeaders = function (containerId) {
    return Array.from(
        document
        .getElementById(containerId)
        .getElementsByClassName('filter-card-header')
    )
}

filterCardHeaderListener = function (e, containerId) {
    e.preventDefault()

    let el = e.target.closest('.filter-card-header')
    // remove is empty for now
    // just trying to get structure in place
    if (el.classList.contains('filter-card-remove')) { 
        removeFilterCard(el, containerId)
    } else {
        toggleFilterCard(el, containerId)
    }
}

removeFilterCard = function (el, containerId) {
    console.log(`remove: ${el} -- ${containerId}`)
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

activateHeaders = function (containerId) {
    let filterHeaders = getHeaders(containerId);
    filterHeaders.forEach((el) => {
        el.addEventListener('click', function (e) {
            filterCardHeaderListener(e, containerId)
        })
    })
}

deactivateHeaders = function (containerId) {
    let filterHeaders = getHeaders(containerId);
    filterHeaders.forEach((el) => {
        el.removeEventListener('click', function (e) {
            filterCardHeaderListener(e, containerId)
        })
    })
}

// need to limit to one event per container, otherwise
//  the events will fight each other (i.e. active -> unactive -> active -> ...)
let cardContainersWithEvents = []

Shiny.addCustomMessageHandler('filter-card-add', function (id) {

    if (cardContainersWithEvents.includes(id)) {
        return
    } else {
        cardContainersWithEvents.push(id)
        document.getElementById(id)
            .addEventListener('click', function (e) {
                    filterCardHeaderListener(e, id)
                })
    }
})
