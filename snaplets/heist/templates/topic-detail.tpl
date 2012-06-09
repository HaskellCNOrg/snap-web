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
      <p><topicAuthor/></p>
      <p><topicContent/></p>
      <p><topicCreateAt/></p>
      <p><topicUpdateAt/></p>
    </article>

    <!-- FIXME: Show me when has authorization.-->
    <ifLoggedIn>
      <div name="topicToolbar">
          <a href="/topicput/${topicId}">Edit</a>
      </div>
    </ifLoggedIn>
    
    <div class="replyPerTopic">
      <replyPerTopic>
        <section class="reply">
          <p><replyContent/></p>
          <p><replyAuthor/></p>
          <p><replyCreateAt/></p>

          <p class="replyOfReply"></p>

          <ifLoggedIn>
            <!-- JS impl in @initReplyToReplyBtn@ -->
            <p><a href="#" id="add-comment-${replyId}" data-topic="${topicId}" data-reply="${replyId}">Add Comment</a></p>
            <a href="/topic/${topicId}/${replyId}/delete/">Delete</a>
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
        $.hcn.initReplyToReplyBtn();
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
