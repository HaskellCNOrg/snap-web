<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="topic">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
        <!-- <dfChildErrorListRef /> -->
         <dfChildErrorList />
      </div>
    </dfIfChildErrors>

    <div class="control-group">
      <dfLabel class="control-label" ref="title"><i18n name="topic.title"/></dfLabel>
      <div class="controls">
        <dfInputText class="input-xxlarge" ref="title" autofocus required />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="content" for="wmd-input"><i18n name="topic.content"/></dfLabel>

      <div class="controls">
        <dfInputTextArea ref="content" class="wmd-input input-xxlarge" id="wmd-input" required />
        <apply template="_markdown-helper" />
        <div id="wmd-preview" class="input-xxlarge wmd-preview"></div>
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="tags"><i18n name="topic.tag"/></dfLabel>
      <div class="controls">
        <dfInputText class="input-xxlarge" ref="tags" />
      </div>
    </div>

    <dfInputHidden ref="tid" />

    <div class="form-actions">
        <i18n name="topic.preview">
          <input class="btn btn-large btn-success" value="${i18nValue}" type="button" id="preview" />
        </i18n>
        <i18n name="topic.submit">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
    </div>

</dfForm>
</div>

<bind tag="bottom-scripts">
  <script type="text/javascript" src="/js/markdown.js"></script>
  <script type="text/javascript" src="/js/topic-form.js"></script>
</bind>

</apply>
