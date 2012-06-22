<apply template="layout">

    <div id="content">

      <ifLoggedIn>
        <a class="btn btn-large btn-primary pull-right" href="/topic">New Topic</a>
      </ifLoggedIn>

      <!-- if count of topics > 0 -->
      <section class="topicList">
        <header>
          <h2>Topics List</h2>
        </header>
        <homeTopics>
          <ul>
            <allTopics>
              <li><a href="/topic/${oid}"><topicTitle/></a>, <topicAuthor/></li>
            </allTopics>
          </ul>
          <div class="pagination">
            <pagination />
          </div>
        </homeTopics>
      </section>

  </div>
  
</apply>
