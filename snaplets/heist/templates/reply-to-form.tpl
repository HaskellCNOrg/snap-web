<dfForm action="/topic/${topicId}/reply">
  <dfIfChildErrors>
    <div class="alert alert-error"> 
      <dfChildErrorList />
    </div>
  </dfIfChildErrors>
  
  <div class="controls">
      <apply template="_markdown-input" />
      <!-- <dfInputTextArea ref="content" class="wmd-input input-xxlarge" id="wmd-input" required /> -->
      <!-- <apply template="_markdown-helper" /> -->
      <!-- <div id="wmd-preview" class="input-xxlarge wmd-preview"></div> -->
  </div>
  
  <dfInputHidden ref="replyToTopicId" value="${topicId}"/>
  
  <i18n name="topic.preview">
    <input class="btn btn-large btn-success" value="${i18nValue}" type="button" id="preview" />
  </i18n>
  <i18n name="reply.add">
    <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
  </i18n>
  
</dfForm>
