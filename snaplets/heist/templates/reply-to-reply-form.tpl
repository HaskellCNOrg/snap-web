
<dfForm id="add-comment-form-${replyid}" action="/topic/${topicid}/${replyid}/reply" role="form">
   
  <dfIfChildErrors>
    <div class="alert alert-error"> 
      <dfChildErrorList />
    </div>
  </dfIfChildErrors>

  <div>
    <dfInputTextArea ref="replyContent" class="wmd-input form-control" rows="4" />
  </div>

  <dfInputHidden ref="replyToTopicId" value="${topicid}"/>
  <dfInputHidden ref="replyToReplyId" value="${replyid}"/>

  <i18n name="reply.reply.submit">
    <dfInputSubmit class="btn btn-default" value="${i18nValue}"></dfInputSubmit>
  </i18n>

</dfForm>
