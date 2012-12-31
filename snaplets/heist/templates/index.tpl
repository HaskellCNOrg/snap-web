<apply template="layout">

    <div id="content">

      <a class="btn btn-large pull-right" href="/topic"><i18n name="topic.new" /></a>

      <!-- if count of topics > 0 -->
      <section class="topic-list">
        <header>
          <h2><i18n name="topic.listHeader" /></h2>
        </header>

        <ifNoTopics>
          <p><i18n name="topic.empty" /></p>
        </ifNoTopics>

        <homeTopics>
          <ol start="${startIndex}">
            <allTopics>
              <li>
                <a href="/topic/${topicId}"><topicTitle/></a> 
                <apply template="_topic-author" />
              </li>
            </allTopics>
          </ol>
          <div class="pagination">
            <pagination />
          </div>
        </homeTopics>
      </section>

  </div>
  
</apply>
