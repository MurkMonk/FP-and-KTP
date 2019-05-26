$(document).ready(function(){
    var yPosition;
    var scrolled = 0;
    var $parallaxElements = $('.parallax_group img');
    var logo = $('#logo');
    $(window).scroll(function() {
        scrolled = $(window).scrollTop();
        for (var i = 0; i < $parallaxElements.length; i++){
            yPosition = (scrolled * 0.15*(i + 1));
            $parallaxElements.eq(i).css({ top: yPosition });
        }
        yPosition = (scrolled * 0.7);
        logo.css({top:yPosition})
    });
});