<apply template="layout">

    <bind tag="subtitle"></bind>

    <div id="content">

      <!-- if count of topics > 0 -->
      <section class="topic-list">
        <!-- <header> -->
        <!--   <h2><i18n name="topic.listHeader" /></h2> -->
        <!-- </header> -->

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
          <div>
            <pagination />
          </div>
        </homeTopics>
      </section>

  </div>

</apply>
