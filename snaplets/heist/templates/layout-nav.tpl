<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">

      <a class="brand" href="/">Happy Snap</a>

      <nav />


      <ul class="nav pull-right">
          <ifLoggedOut>
          <li><a href="/signin">Sign in</a></li>
          <li><a href="/signup">Sign up</a></li>
          </ifLoggedOut> 

        <!-- <form class="navbar-search pull-left" action="/noImplYet">
          <input type="text" class="search-query span2" placeholder="Search">
        </form>
         -->

          <ifLoggedIn>
          <li><a href="/setting"><loggedInUser/></a></li>
          <li><a href="/signout">Sign out</a></li>
          </ifLoggedIn>

          <li><a href="/abount">About</a></li>

      </ul>
    </div>
  </div>
</div>

