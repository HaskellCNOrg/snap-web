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

  <div class="topic-content" itemscope itemtype="http://schema.org/Article">

    <article class="topic">
      <h1 itemprop="name"><topicTitle /></h1>
      <div itemprop="articleBody"><topicContent/></div>
    </article>

    <apply template="_topic-author" />

    <p class="tag-list">
        <topicTagList>
            <apply template="_single-tag" />
        </topicTagList>
    </p>

    <ifLoggedIn>
      <topicEditable>
        <div class="btn-group topic-toolbar">
          <a class="btn btn-link" href="/topicput/${topicId}"><i18n name="site.edit" /></a>
          <a class="btn btn-link" href="/topicdelete/${topicId}"><i18n name="site.delete" /></a>
          <!-- FIXME: confirmation when deletion -->
        </div>
      </topicEditable>
    </ifLoggedIn>


    <div class="reply-per-topic">
      <h3><i18n name="topic.answers" /></h3>
      <replyPerTopic>
        <section class="reply">
          <p><replyContentMD/></p>
          <apply template="_reply-author" />

          <div class="reply-of-reply">
            <apply template="reply-to-reply-detail" />
          </div>

          <ifLoggedIn>
            <!-- JS impl in @initReplyToReplyBtn@ -->
            <a class="btn btn-link invisible" id="add-comment-${replyId}" data-topic="${topicId}" data-reply="${replyId}"><i18n name="reply.reply.add" /></a>
          </ifLoggedIn>

        </section>

      </replyPerTopic>
    </div>

  </div>

  <ifLoggedIn>
      <hr />
      <div class="reply-editor">
          <h3><i18n name="reply.add" /></h3>
          <apply template="reply-to-form" />
      </div>
  </ifLoggedIn>

</ifFound>

<bind tag="bottom-scripts">
  <script type="text/javascript" src="/js/markdown.js"></script>
  <script type="text/javascript" src="/js/topic.js"></script>
</bind>

</apply>
