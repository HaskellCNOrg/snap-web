<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">

      <a class="brand" href="/"><i18n name="site.name" /></a>

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
              <a target="_blank" href="http://www.haskellcn.org/study.html"><i18n name="site.study" /></a>
          </li>
          <ifLoggedOut>
          <li><a href="/signin"><i18n name="user.signin" /></a></li>
          <li><a href="/signup"><i18n name="user.signup" /></a></li>
          </ifLoggedOut> 

          <ifLoggedIn>
          <li><a href="/user"><currentUser/></a></li>
          <li><a href="/signout"><i18n name="user.signout" /></a></li>
          </ifLoggedIn>

      </ul>
    </div>
  </div>
</div>

