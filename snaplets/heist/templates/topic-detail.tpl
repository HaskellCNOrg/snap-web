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
  <h2>
    <topicTitle />
  </h2>
  
  <p><topicAuthor/></p>
  <p><topicContent/></p>
  <p><topicCreateAt/></p>
  <p><topicUpdateAt/></p>

  </ifTopic>

</apply>
