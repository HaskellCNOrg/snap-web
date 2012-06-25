
<dfForm id="add-comment-form-${replyid}" action="/topic/${topicid}/${replyid}/reply">
   
     <dfIfChildErrors>
        <div class="alert alert-error"> 
           <dfChildErrorList />
        </div>
      </dfIfChildErrors>

  <dfInputTextArea ref="replyContent" />

  <dfInputHidden ref="replyToTopicId" value="${topicid}"/>
  <dfInputHidden ref="replyToReplyId" value="${replyid}"/>

  <i18n name="reply-to-reply-submit">
    <dfInputSubmit value="${i18nValue}"></dfInputSubmit>
  </i18n>

</dfForm>
