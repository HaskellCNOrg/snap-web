<apply template="layout">

    <div id="content">

      <a class="btn btn-large btn-primary pull-right" href="/topic"><i18n name="topic-new" /></a>

      <!-- if count of topics > 0 -->
      <section class="topicList">
        <header>
          <h2><i18n name="topic-list-header" /></h2>
        </header>
        <homeTopics>
          <ul>
            <allTopics>
              <li><a href="/topic/${topicId}"><topicTitle/></a> <p class="author">Submited by <topicAuthor/> at <topicCreateAt /></p></li>
            </allTopics>
          </ul>
          <div class="pagination">
            <pagination />
          </div>
        </homeTopics>
      </section>

  </div>
  
</apply>
