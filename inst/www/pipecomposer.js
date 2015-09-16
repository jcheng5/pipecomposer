// Sets the active stage, which changes its appearance and sends
// the input value "activeStage" (a number, or null) to the server.
function activateStage(el) {
  $(".stage").removeClass("active");
  if (el && Shiny.onInputChange) {
    $(el).addClass("active");
    var i = parseInt(el.id.replace(/^stage/, ""));
    Shiny.onInputChange("activeStage", i);
  }
  else {
    Shiny.onInputChange("activeStage", null);
  }
}

// A stage that's entered or focused becomes the active one
$(document).on("mouseenter focus", ".stage", function(e) {
  activateStage(e.target);
});

// Double-click on #code selects the entire text
$(document).on("dblclick", "#code", function(e) {
  var selection = window.getSelection();
  var range = document.createRange();
  range.selectNodeContents(e.target);
  selection.removeAllRanges();
  selection.addRange(range);
});
// Deselect text automatically on mouse leave
$(document).on("mouseleave", "#code", function(e) {
  var selection = window.getSelection();
  if (selection.focusNode.parentNode == e.target &&
      selection.anchorNode.parentNode == e.target) {
    selection.removeAllRanges();
  }
});
