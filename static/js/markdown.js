var markdown = (function ($) {
   var previewS = '#preview',
       contentS = '#wmd-input',
       previewPanelS = '#wmd-preview',
       prevCallback = function (res) {
          $(previewPanelS).html(res);
       },
       previewHandler = function (e) {
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
