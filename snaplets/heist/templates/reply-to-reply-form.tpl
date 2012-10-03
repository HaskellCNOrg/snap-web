
<dfForm id="add-comment-form-${replyid}" action="/topic/${topicid}/${replyid}/reply">
   
  <dfIfChildErrors>
    <div class="alert alert-error"> 
      <dfChildErrorList />
    </div>
  </dfIfChildErrors>

  <div class="wmd-panel">
    <dfInputTextArea ref="replyContent" class="wmd-input" />
  </div>
  

  <dfInputHidden ref="replyToTopicId" value="${topicid}"/>
  <dfInputHidden ref="replyToReplyId" value="${replyid}"/>

  <i18n name="reply.reply.add">
    <dfInputSubmit value="${i18nValue}"></dfInputSubmit>
  </i18n>

</dfForm>
