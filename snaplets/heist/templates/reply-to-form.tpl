<dfForm action="/topic/${topicId}/reply">
  <dfIfChildErrors>
    <div class="alert alert-error"> 
      <dfChildErrorList />
    </div>
  </dfIfChildErrors>
  
  <div class="wmd-panel">
    <div id="wmd-button-bar"></div>
    <dfInputTextArea ref="replyContent" class="wmd-input" id="wmd-input" required />
  </div>
  <div id="wmd-preview" class="wmd-panel wmd-preview"></div>
  
  <dfInputHidden ref="replyToTopicId" value="${topicId}"/>
  
  <i18n name="topic.preview">
    <input class="btn btn-large btn-success" value="${i18nValue}" type="button" id="preview" />
  </i18n>
  <i18n name="reply.add">
    <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
  </i18n>
  
</dfForm>
