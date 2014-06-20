function activateStage(el) {
  $(".stage").removeClass("active");
  if (el) {
    $(el).addClass("active");
    var i = parseInt(el.id.replace(/^stage/, ""));
    Shiny.onInputChange("activeStage", i);
  }
  else {
    console.log('b')
    Shiny.onInputChange("activeStage", null);
  }
}

$(function() {
  $(document).on("mouseenter focus", ".stage", function(e) {
    var el = e.target;
    activateStage(el);
  });
});
