;(function ($) {
   var previewS = '#preview',
       contentS = '#wmd-input',
       previewPanelS = '#wmd-preview',
       prevCallback = function (res) {
          $(previewPanelS).html(res);
       },
       previewHandler = function (e) {
          //console.log("this is:", e.target);
          $.post('/topic/preview',
                 { 'content': $(contentS).val() })
           .then(prevCallback, prevCallback);
       },
       bindPreview = function () {
          $(previewS).click(previewHandler);
       };

   bindPreview();

})(jQuery);


$(function () {
   $('div.alert-error li').each(function (index, x) {
      $("[name$=" + $(x).attr("data-error") + "]").closest("div.control-group").addClass("error");
   });
});
