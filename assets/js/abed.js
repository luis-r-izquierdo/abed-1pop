$(document).ready(function () {

   /*$(".dropdown-submenu").click(function (event) {
   // stop bootstrap.js to hide the parents
   event.stopPropagation();
   // hide the open children
   $(this).find(".dropdown-submenu").removeClass('open');
   // add 'open' class to all parents with class 'dropdown-submenu'
   $(this).parents(".dropdown-submenu").addClass('open');
   // this is also open (or was)
   $(this).toggleClass('open');
});

// this event fires when the dropdown hidden.
$('.dropdown').on('hidden.bs.dropdown', function () {
   $(".dropdown").find(".dropdown-submenu").removeClass('open');
});

// The following makes menus disappear when you've clicked on one item
$(function () {
   $('.navbar-collapse ul li a:not(.dropdown-toggle)').click(function () {
      $('.navbar-toggle:visible').click();
   });
});

$(function () {
   $('.navbar-collapse ul li a:not(.dropdown-toggle)').bind('click touchstart', function () {
      $('.navbar-toggle:visible').click();
   });
});

// http://stackoverflow.com/questions/17579750/drop-down-menu-not-working-on-mobile-devices
$('.dropdown-toggle').click(function (e) {
   e.preventDefault();
   setTimeout($.proxy(function () {
      if ('ontouchstart' in document.documentElement) {
         $(this).siblings('.dropdown-backdrop').off().remove();
      }
   }, this), 0);
});*/



   $('.dropdown-submenu a.dropdown-toggle').on("click", function (e) {
      $(this).next('ul').toggle();
      e.stopPropagation();
      e.preventDefault();
   });

   $('body').scrollspy({
      target: "sidebar"
   })

});