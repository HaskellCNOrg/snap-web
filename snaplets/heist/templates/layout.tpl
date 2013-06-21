<!DOCTYPE html>
<html lang="zh">
  <head>
    <meta charset="utf-8" />
    <title><i18n name="site.name" /></title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <i18n name="site.description">
      <meta name="description" content="${i18nValue}"/>
    </i18n>
    <i18n name="site.keywords">
      <meta name="keywords" content="${i18nValue}"/>
    </i18n>
    <meta name="author" content="haskellcn.org"/>

    <link href="/feed/topic" type="application/atom+xml" rel="alternate" title="Topics Feed" />
    <link href="/feed/comment" type="application/atom+xml" rel="alternate" title="Comments Feed" />

    <apply template="_layout-css" />

    <!--[if lt IE 9]>
      <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
    <apply template="_google-a" />

  </head>

  <body>
      <apply template="_layout-nav"/>

      <div class="container">
          <apply-content />
          <apply template="_layout-footer" />
      </div>

      <apply template="_layout-js" />
      <bottom-scripts />

  </body>
</html>
