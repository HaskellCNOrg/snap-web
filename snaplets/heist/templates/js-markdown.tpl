
<showOnEnv on="prod">
  <script type="text/javascript" src="/js/Markdown.js"></script>
</showOnEnv>
<showOnEnv on="devel">
  <script type="text/javascript" src="/js/Markdown.Converter.js"></script>
  <script type="text/javascript" src="/js/Markdown.Sanitizer.js"></script>
  <script type="text/javascript" src="/js/Markdown.Editor.js"></script>
</showOnEnv>


<static>
  <script>
    (function () {
      var converter1 = Markdown.getSanitizingConverter();
      var editor1 = new Markdown.Editor(converter1);
      editor1.run();
    })();
  </script>
</static>
