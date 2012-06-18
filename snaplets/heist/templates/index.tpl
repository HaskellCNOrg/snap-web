<apply template="layout">

    <div id="content">

      <ifLoggedIn>
        <a class="btn btn-primary" href="/topic">New Topic</a>
      </ifLoggedIn>

      <h2>Topics List</h2>
      <!-- if count of topics > 0 -->
      <p>
       <allTopics>
         <ul>
           <li><a href="/topic/${oid}"><topicTitle/></a>, <topicAuthor/></li>
         </ul>
       </allTopics>
       <pagination />
      </p>

  </div>
  
</apply>
