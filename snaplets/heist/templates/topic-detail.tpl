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

  <div class="topic-content">

    <article class="topic">
      <h2><topicTitle /></h2>
      <p><topicContent/></p>
    </article>

    <apply template="_topic-author" />

    <ifLoggedIn>
      <topicEditable>
        <div class="btn-group topic-toolbar">
            <a class="btn btn-link" href="/topicput/${topicId}"><i18n name="site.edit" /></a>
        </div>
      </topicEditable>
    </ifLoggedIn>

    <p>
        <topicTagList>
            <apply template="_single-tag" />
        </topicTagList>
    </p>

    <div class="reply-per-topic">
      <h3><i18n name="topic.answers" /></h3>
      <replyPerTopic>
        <section class="reply">
          <p><replyContentMD/></p>
          <p class="author">
            <a href="/user/${replyAuthorId}"><replyAuthor/></a> <replyCreateAt/>
            <apply template="_delete-reply" />
          </p>

          <div class="reply-of-reply">
            <apply template="reply-to-reply-detail" />
          </div>

          <ifLoggedIn>
            <!-- JS impl in @initReplyToReplyBtn@ -->
            <p>
              <a class="btn btn-link" id="add-comment-${replyId}" data-topic="${topicId}" data-reply="${replyId}"><i18n name="reply.reply.add" /></a>
            </p>
          </ifLoggedIn>
          
        </section>

      </replyPerTopic>
    </div>

  </div>
  
  <ifLoggedIn>
      <hr />
      <div class="reply-editor">
          <h3>Add Reply</h3>
          <apply template="reply-to-form" />
      </div>
  </ifLoggedIn>

</ifFound>

<bind tag="bottom-scripts">
  <script type="text/javascript" src="/js/markdown.js"></script>
  <script type="text/javascript" src="/js/topic.js"></script>
</bind>
  
</apply>
