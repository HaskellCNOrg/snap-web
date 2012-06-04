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
    <article>
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
        <a href="/topicput/${oid}">Edit</a>
    </div>
    </ifLoggedIn>
  </div>
  
  <ifLoggedIn>
  <div class="replyMain">
    <dfForm action="/reply">
      <dfIfChildErrors>
        <div class="alert alert-error"> 
           <dfChildErrorList ref=""></dfChildErrorList>
        </div>
      </dfIfChildErrors>

      <div class="wmd-panel">
        <div id="wmd-button-bar"></div>
        <dfInputTextArea ref="content" class="wmd-input" id="wmd-input" required />
      </div>
      <div id="wmd-preview" class="wmd-panel wmd-preview"></div>

      <dfInputHidden ref="replyToTopicId" value="${oid}"/>

      <i18n name="reply-submit">
        <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
      </i18n>

    </dfForm>
  </div>
  </ifLoggedIn>

</ifFound>

  <bind tag="bottom-scripts">
    <apply template="js-markdown" />
  </bind>
  
</apply>



 <!-- FIXME: show me when user login
  
  <ifLoggedIn>  
    <a href="#">Share</a>
    <a href="/favorite">Save</a>
  </ifLoggedIn>
  
  -->
