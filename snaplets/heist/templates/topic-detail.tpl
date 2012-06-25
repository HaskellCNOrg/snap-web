<apply template="layout">

<ifNotFound>
  <div class="alert alert-error"> 
    <ul>
      <li><exceptionValue/></li>
    </ul>
  </div>
  <returnToHome />
</ifNotFound>

<ifFound>

  <div class="topicMain">

    <article class="topic">
      <h2>
        <topicTitle />
      </h2>
      <p><topicContent/></p>

      <p class="author">Add by <topicAuthor/> at <topicCreateAt/></p>

      <!-- <p><topicUpdateAt/></p> -->
    </article>

    <!-- FIXME: Show me when has authorization.-->
    <ifLoggedIn>
      <topicEditable>
        <div name="topicToolbar">
            <a href="/topicput/${topicId}"><i18n name="site-edit" /></a>
        </div>
      </topicEditable>
    </ifLoggedIn>

    <div class="replyPerTopic">
      <h3><i18n name="topic-answers" /></h3>
      <replyPerTopic>
        <section class="reply">
          <p><replyContent/></p>
          <p class="author">Add by <replyAuthor/> at <replyCreateAt/></p>

          <div class="replyOfReply">
            <apply template="reply-to-reply-detail" />
          </div>

          <ifLoggedIn>
            <!-- JS impl in @initReplyToReplyBtn@ -->
            <p>
              <a id="add-comment-${replyId}" data-topic="${topicId}" data-reply="${replyId}"><i18n name="topic-add-reply" /></a>
            </p>
            <!-- <a href="/topic/${topicId}/${replyId}/delete/">Delete</a> -->
          </ifLoggedIn>
          
        </section>

      </replyPerTopic>
    </div>

  </div>
  
  <ifLoggedIn>
    <div class="replyEditor">
       <apply template="reply-to-form" />
    </div>
  </ifLoggedIn>

</ifFound>

  <bind tag="bottom-scripts">
    <apply template="js-markdown" />
    <script type="text/javascript">
      $(function () {
        $.hcn.initReplyToReply();
      })
    </script>
  </bind>
  
</apply>



 <!-- FIXME: show me when user login
  
  <ifLoggedIn>  
    <a href="#">Share</a>
    <a href="/favorite">Save</a>
  </ifLoggedIn>
  
  -->
