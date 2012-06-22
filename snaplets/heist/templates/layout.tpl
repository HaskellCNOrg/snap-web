<!DOCTYPE html>
<html>
  <head>
    <title><i18n name="site-name" /></title>
    <meta name="author" content="freizl"/>
    <meta name="description" content="haskell chinese community"/>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

    <showOnEnv on="prod">
    <!-- =========================================== -->
    <!-- FOR PRODUCTION -->
    <link rel="stylesheet" href="/css/main.css" />
    <!-- =========================================== -->
    </showOnEnv>

    <showOnEnv on="devel">
    <!-- =========================================== -->
    <!-- FOR DEVELOPMENT -->
    <link rel="stylesheet/less" href="/less/bootstrap.less"/>
    <script src="/lessjs/less-1.3.0.min.js"></script>
    <!-- =========================================== -->
    </showOnEnv>

  </head>
  
  <body>
      <apply template="layout-nav"/>

      <div class="container">
          <content />
          <apply template="layout-footer" />
      </div>

      <script type="text/javascript" src="http://code.jquery.com/jquery-1.7.2.min.js"></script>
      <script type="text/javascript" src="/js/main.js"></script>
      <bottom-scripts />

  </body>
</html>
