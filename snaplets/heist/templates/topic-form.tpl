<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="topic">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <div class="control-group">
      <dfLabel class="control-label" ref="title"><i18n name="topic-title"/></dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="title" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="content" for="wmd-input"><i18n name="topic-content"/></dfLabel>

      <div class="controls">
        <div class="wmd-panel">
          <div id="wmd-button-bar"></div>
          <dfInputTextArea ref="content" class="wmd-input" id="wmd-input" />
        </div>
        <div id="wmd-preview" class="wmd-panel wmd-preview"></div>
      </div>
    </div>

    <dfInputHidden ref="tid" />

    <div class="form-actions">
        <i18n name="topic-submit">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
    </div>

</dfForm>
</div>

<bind tag="bottom-scripts">
  <script type="text/javascript" src="/js/Markdown.Converter.js"></script>
  <script type="text/javascript" src="/js/Markdown.Sanitizer.js"></script>
  <script type="text/javascript" src="/js/Markdown.Editor.js"></script>
  <script>
    (function () {
      var converter1 = Markdown.getSanitizingConverter();
      var editor1 = new Markdown.Editor(converter1);
      editor1.run();
    })();
  </script>
  </bind>

</apply>
