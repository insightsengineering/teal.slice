// need shiny:inputchanged b/c adding to header via session$onFlushed is too soon
//  and elements don't exist yet => cant' add a listener
//  (at least on bs 3, 4)
$(document).on('shiny:inputchanged', function(event) {
  if (event.name.endsWith('enable') || event.name.endsWith('remove')) {
    // this works for bootstrap 3 and 4
    // document.getElementById(event.name).addEventListener('click', function(e){
    //   e.stopPropagation()
    // })

    // this works for bootstrap 5
    let el = document.getElementById(event.name)
    let body = el.closest(".card-header").nextElementSibling
    body.addEventListener('show.bs.collapse', function(event) {
      disableClicked = event.explicitOriginalTarget.id.includes("enable")
      console.log(disableClicked)
      if (disableClicked) {
        event.preventDefault()
      }
    })
  }
})
