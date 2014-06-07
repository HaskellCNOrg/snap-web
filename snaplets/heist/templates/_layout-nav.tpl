<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">

      <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </a>

      <a class="brand" href="/"><i18n name="site.name" /></a>


      <div class="nav-collapse collapse" style="height: 0; ">

        <ul class="nav">
          <li>
              <a href="/topic"><i18n name="topic.new" /></a>
          </li>
          <li>
            <a href="/tags"><i18n name="tag.list" /></a>
          </li>
        </ul>

        <ul class="nav pull-right">
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
      </div>
    </div>
  </div>
</div>
<div>
<a href="https://github.com/HaskellCNOrg/snap-web.git" target="_blank">
    <img style="position: absolute; top: 42px; right: 0; border: 0" src="https://i.alipayobjects.com/e/201211/1dbSqT9ykm.png" width="149" height="149" alt="Fork me on GitHub">
    </a>
</div>
