<apply template="layout">

  <ifTopicError>
    <div class="alert alert-error"> 
      <ul>
        <li><exceptionValue/></li>
      </ul>
    </div>
    <returnToHome />
  </ifTopicError>

  <ifTopic>

  <article>
    <h2>
      <topicTitle />
    </h2>
    <p><topicAuthor/></p>
    <p><topicContent/></p>
    <p><topicCreateAt/></p>
    <p><topicUpdateAt/></p>
  </article>

  <!-- Show me when auther login -->
  <div name="topicToolbar">
      <a href="/topicput/${oid}">Edit</a>

<!--     
      <a href="#">Share</a>
<a href="/favorite">Save</a>
    <a href="/topicPut">Delete</a> -->
  </div>


  </ifTopic>

</apply>
