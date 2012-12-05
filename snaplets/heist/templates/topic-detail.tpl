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
      <h2><topicTitle /></h2>
      <p><topicContent/></p>

      <div class="small-font">
        <a href="/user/${topicAuthorId}"><topicAuthor/></a> 
        <span><topicCreateAt/></span>
        <ul class="tags">
          <topicTagList>
            <li><a href="/tag/${tagId}"><tagName/></a></li>
          </topicTagList>
        </ul>
      </div>
      <!-- <p><topicUpdateAt/></p> -->
    </article>

    <ifLoggedIn>
      <topicEditable>
        <div name="topicToolbar" class="topic-toolbar">
            <a href="/topicput/${topicId}"><i18n name="site.edit" /></a>
        </div>
      </topicEditable>
    </ifLoggedIn>

    <div class="replyPerTopic">
      <h3><i18n name="topic.answers" /></h3>
      <replyPerTopic>
        <section class="reply">
          <p><replyContentMD/></p>
          <p class="author"><a href="/user/${replyAuthorId}"><replyAuthor/></a> <replyCreateAt/></p>

          <div class="replyOfReply">
            <apply template="reply-to-reply-detail" />
          </div>

          <ifLoggedIn>
            <!-- JS impl in @initReplyToReplyBtn@ -->
            <p>
              <a id="add-comment-${replyId}" data-topic="${topicId}" data-reply="${replyId}"><i18n name="reply.reply.add" /></a>
            </p>
            <isCurrentUserAdmin>
              <a href="/topic/${topicId}/${replyId}/delete/">Delete</a>
            </isCurrentUserAdmin>
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
  <script type="text/javascript" src="/js/markdown.js"></script>
  <script type="text/javascript" src="/js/topic.js"></script>
</bind>
  
</apply>
