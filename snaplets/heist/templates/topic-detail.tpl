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

  <div name="topicMain">
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
  <div name="commentMain">
    <dfForm class="form-horizontal" action="/comment">
    <div class="control-group">

      <div class="controls">
        <div class="wmd-panel">
          <div id="wmd-button-bar"></div>
          <dfInputTextArea ref="content" class="wmd-input" id="wmd-input" />
        </div>
        <div id="wmd-preview" class="wmd-panel wmd-preview"></div>
      </div>
    </div>
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
