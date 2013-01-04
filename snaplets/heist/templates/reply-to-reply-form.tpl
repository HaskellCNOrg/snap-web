
<dfForm id="add-comment-form-${replyid}" action="/topic/${topicid}/${replyid}/reply">
   
  <dfIfChildErrors>
    <div class="alert alert-error"> 
      <dfChildErrorList />
    </div>
  </dfIfChildErrors>

  <div>
    <dfInputTextArea ref="replyContent" class="wmd-input input-xxlarge" />
  </div>

  <dfInputHidden ref="replyToTopicId" value="${topicid}"/>
  <dfInputHidden ref="replyToReplyId" value="${replyid}"/>

  <i18n name="reply.reply.submit">
    <dfInputSubmit class="btn" value="${i18nValue}"></dfInputSubmit>
  </i18n>

</dfForm>
