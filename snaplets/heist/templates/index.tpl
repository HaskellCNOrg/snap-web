<apply template="layout">

    <div id="content">

      <h2>
        <a href="/topic">New Topic</a>
      </h2>

      <h2>Topics List</h2>
      <!-- if count of topics > 0 -->
      <p>
       <allTopics>
         <ul>
           <li><a href="/topic/${oid}"><topicTitle/></a></li>
         </ul>
       </allTopics>
      </p>

  </div>
  
</apply>
