<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta name="author" content="haskellcn.org"/>
    <i18n name="site.description">
      <meta name="description" content="${i18nValue}"/>
    </i18n>
    <title><i18n name="site.name" /></title>

    <link href="/feed/topic" type="application/atom+xml" rel="alternate" title="Topics Feed" />
    <link href="/feed/comment" type="application/atom+xml" rel="alternate" title="Comments Feed" />

    <apply template="_layout-css" />
    <apply template="_google-a" />

  </head>
  
  <body>
      <apply template="_layout-nav"/>

      <div class="container">
          <content />
          <apply template="_layout-footer" />
      </div>

      <script type="text/javascript" src="/js/libs/jquery.js"></script>
      <script type="text/javascript" src="/js/libs/bootstrap-button.js"></script>
      <bottom-scripts />

  </body>
</html>
