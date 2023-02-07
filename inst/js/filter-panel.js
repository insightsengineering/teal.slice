filterCardListener = function (e) {
    let target = e.target;
    // e.currentTarget takes into account bubbling
    // i.e. it's the element the listener is actually attached to
    let container = e.currentTarget
    let containerId = container.id
    

    // remove icon is nested with 'a' tag (actionLink) so need to check both 
    // 'a' tag and 'i' tag
    let removeCard = target.classList.contains("filter-card-remove") ||
        target.parentElement.classList.contains("filter-card-remove")

    if (removeCard) {
        // only remove listener if container is empty
        let numCardsInContainer = getHeaders(containerId).length
        if (numCardsInContainer === 0) {
            container.removeEventListener('click', filterCardListener)
        }

    } else {
        let header = target.closest('.filter-card-header')
        // if header is null, body was clicked (e.g. an input)
        if (header !== null) {
            toggleFilterCard(header, containerId)
        }
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
        let activeSummary = activeHeader.querySelector(".filter-card-summary")
        let activeBody = activeHeader.nextElementSibling;

        activeHeader.classList.remove("active")
        activeSummary.style.display = "block";
        activeBody.style.display = "none";
        activeBody.style.maxHeight = null; //animation

    }
    
    if (!clickedCardIsActive) {

        let cardHeader = el;
        let cardSummary = el.querySelector(".filter-card-summary");
        let cardBody = cardHeader.nextElementSibling;

        cardHeader.classList.add("active")
        cardSummary.style.display = "none"
        cardBody.style.display = "block"

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
    document.getElementById(id)
        .addEventListener('click', filterCardListener)
})
