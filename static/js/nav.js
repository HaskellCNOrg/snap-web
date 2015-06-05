;(function ($, location) {
	$('.navbar a').map(function () {
		if ($(this).attr('href') === location.pathname) { $(this).parent().addClass('active');}
	    }); 

}(jQuery, window.location))