var markdown = (function ($) {
   var previewS = '#preview',
       contentS = '#wmd-input',
       previewPanelS = '#wmd-preview',

       scrollToPreview = function (x) {
             $('html, body').animate({
                scrollTop: $(previewPanelS).offset().top - 80
             }, 1500);
       },

       prevCallback = function (res) {
          $(previewS).button('reset');
          $(previewPanelS).html(res);
          scrollToPreview();
       },

       previewHandler = function (e) {
          var that = this;
          $(that).button('loading');
          $.post('/topic/preview',
                 { 'content': $(contentS).val() })
           .then(prevCallback, prevCallback);
       },

       bindPreview = function () {
          $(previewS).click(previewHandler);
       };

   return {
      init: function () { bindPreview();}
   };

})(jQuery);

$(function () {
   markdown.init();
});
