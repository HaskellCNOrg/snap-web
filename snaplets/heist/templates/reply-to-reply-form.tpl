
<dfForm id="add-comment-form-${replyId}" action="/replytoreply">
   
     <dfIfChildErrors>
        <div class="alert alert-error"> 
           <dfChildErrorList />
        </div>
      </dfIfChildErrors>

  <dfInputTextArea ref="replyToReplyContent" />

  <dfInputHidden ref="replyToReplyReplyId" value="${replyId}"/>
  <dfInputHidden ref="replyToReplyTopicId" value="${topicId}"/>

  <i18n name="reply-to-reply-submit">
    <dfInputSubmit value="${i18nValue}"></dfInputSubmit>
  </i18n>

</dfForm>
