<div class="navbar navbar-default navbar-fixed-topnav" role="navigation">
    <div class="container">

        <div class="navbar-header">
          <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>

          <a class="navbar-brand" href="/"><i18n name="site.name" /></a>

        </div>
        <div class="navbar-collapse collapse">
          <ul class="nav navbar-nav">
              <li>
                  <a href="/topic"><i18n name="topic.new" /></a>
              </li>
              <li>
                  <a href="/tags"><i18n name="tag.list" /></a>
              </li>
          </ul>

          <ul class="nav navbar-nav navbar-right">
              <li>
                  <a target="_blank" href="http://haskellnews.org/grouped"><i18n name="site.haskellnews" /></a>
              </li>
              <li>
                  <a target="_blank" href="http://www.haskellcn.org/study.html"><i18n name="site.study" /></a>
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
        <img style="position: fixed; top: 52px; right: 0; border: 0; width: 149px; height: 149px; z-index:2000;" src="http://aral.github.com/fork-me-on-github-retina-ribbons/right-graphite@2x.png" alt="Fork me on GitHub">
    </a>
</div>
