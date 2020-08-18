(function ($) {
    'use strict';

    // Preloader js
    $(window).on('load', function () {
      $('.preloader').fadeOut(1000);
    });

    // Avoid indefinitely preloading, transition
    // still with more discrete visual cues that the page is still
    setTimeout(function () {
      $('.preloader').animate({
        opacity: 0.3,
        top: "55px",
        left: "5px",
        width: "66px",
        height: "50px"
      }, 1000, function () {
        $('.preloader').addClass("preloader2");
      });
    }, 1500);
})(jQuery);