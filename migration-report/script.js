$(function () {
$('dt.collapsible').click(function(e){
   if ($(e.target).closest("a").length == 0)
      $(this).toggleClass("collapsed");
});
$('dt.collapsible').toggleClass("collapsed").toggleClass("active",true);
//var expand_button = $("<a class=\"expand\" href=\"#\">[Expand all]</a>");
var expand_button = $("<button class=\"expand\">Expand all</button>");
var collapse = false;
expand_button.click(function(e){
  $('dt.collapsible').toggleClass("collapsed",collapse);
  if (collapse) {
    $(this).text("Expand all");
  } else {
    $(this).text("Collapse all");
  }
  collapse = !collapse;
  e.prevendDefault();
});
expand_button.insertBefore("dl:first");
});
