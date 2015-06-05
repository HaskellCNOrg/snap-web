<div class="navbar navbar-default navbar-fixed-top" role="navigation">
    <div class="container">

        <div class="navbar-header">
          <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>

          <i18n name="site.name" >
              <a class="navbar-brand" href="/" title="${i18nValue}" alt="${i18nValue}"><i18nValue/></a>
          </i18n>


        </div>
        <div class="navbar-collapse collapse">
          <ul class="nav navbar-nav">
              <li>
                  <a href="/topic"><i18n name="topic.new" /></a>
              </li>
              <li>
                  <i18n name="tag.list">
                      <a title="${i18nValue}" alt="${i18nValue}" href="/tags"><i18nValue/></a>
                  </i18n>

              </li>
          </ul>

          <ul class="nav navbar-nav navbar-right">
              <li>
                  <a target="_blank" href="http://haskellnews.org/grouped"><i18n name="site.haskellnews" /></a>
              </li>
              <li>
                  <i18n name="site.study">
                      <a title="${i18nValue}" alt="${i18nValue}" href="/study"><i18nValue/></a>
                  </i18n>

              </li>

              <ifLoggedOut>
                  <li><a href="/signin${goto}"><i18n name="user.signin" /></a></li>
                  <li><a href="/signup${goto}"><i18n name="user.signup" /></a></li>
              </ifLoggedOut>

              <ifLoggedIn>
                  <li><a href="/user"><currentUser/></a></li>
                  <li><a href="/signout"><i18n name="user.signout" /></a></li>
              </ifLoggedIn>
          </ul>
        </div><!--/.nav-collapse -->
      </div>
</div>

<div>
    <a href="https://github.com/HaskellCNOrg/snap-web.git" target="_blank">
        <img id="fork-from-github" style="position: fixed; top: 52px; right: 0; border: 0; width: 149px; height: 149px; z-index:2000;" src="http://aral.github.com/fork-me-on-github-retina-ribbons/right-graphite@2x.png" alt="Fork me on GitHub">
    </a>
</div>
