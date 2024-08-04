function setAndRemoveClass(element, setClass, removeClass) {
  if (typeof element === "string") {
    element = document.querySelector(element);
  }
  element.classList.add(setClass);
  element.classList.remove(removeClass);
}

function toggleClass(element, class1, class2) {
  if (typeof element === "string") {
    element = document.querySelector(element);
  }
  if (element.classList.contains(class1)) {
    setAndRemoveClass(element, class2, class1);
  } else {
    setAndRemoveClass(element, class1, class2);
  }
}

function showPanelItem(targeSelector, duration = 400, easing = "slideInTop") {
  $(`#${targeSelector}`).show(duration, easing);
  $(`#${targeSelector}`).trigger("shown");
}

function hidePanelItem(targeSelector, duration = 400, easing = "slideOutLeft") {
  $(`#${targeSelector}`).hide(duration, easing);
}

function setTitle(targeSelector, title) {
  $(`#${targeSelector}`).attr("title", title);
}

function toggleTitle(targeSelector, title1, title2) {
  var currentTitle = $(`#${targeSelector}`).attr("title");
  var newTitle = currentTitle === title1 ? title2 : title1;
  setTitle(targeSelector, newTitle);
}

// when invoked it hides/shows targeSelectors elements and changes class of element from class1 <-> class2
function togglePanelItem(
  element,
  targetSelectors,
  class1,
  class2,
  duration = 400,
  easing = "swing"
) {
  if (!Array.isArray(targetSelectors)) {
    targetSelectors = [targetSelectors];
  }

  targetSelectors.forEach((targetSelector) => {
    if ($(`#${targetSelector}`).is(":visible")) {
      hidePanelItem(targetSelector, duration, easing);
    } else {
      showPanelItem(targetSelector, duration, easing);
    }
  });

  toggleClass(element, class1, class2);
}

function clickWhenClassPresent(
  targetSelector,
  className,
  additionalCondition = true
) {
  if ($(`#${targetSelector}`).hasClass(className) && additionalCondition) {
    $(`#${targetSelector}`).trigger("click");
  }
}
