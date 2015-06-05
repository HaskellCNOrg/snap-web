<dfForm action="/topic/${topicId}/reply" role="form">
  <dfIfChildErrors>
    <div class="alert alert-error"> 
      <dfChildErrorList />
    </div>
  </dfIfChildErrors>
  
  <div class="form-group">
      <apply template="_markdown-input" />
  </div>
  
  <dfInputHidden ref="replyToTopicId" value="${topicId}"/>
  
  <i18n name="topic.preview">
    <input class="btn btn-lg btn-success" value="${i18nValue}" type="button" id="preview" />
  </i18n>
  <i18n name="reply.add">
    <dfInputSubmit class="btn btn-lg btn-default" value="${i18nValue}"></dfInputSubmit>
  </i18n>
  
</dfForm>
