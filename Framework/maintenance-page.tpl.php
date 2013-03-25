<!DOCTYPE html>
<!--[if lt IE 7]> <html class="ie6 ie" lang="<?= $language->language ?>" dir="<?= $language->dir ?>"> <![endif]-->
<!--[if IE 7]>    <html class="ie7 ie" lang="<?= $language->language ?>" dir="<?= $language->dir ?>"> <![endif]-->
<!--[if IE 8]>    <html class="ie8 ie" lang="<?= $language->language ?>" dir="<?= $language->dir ?>"> <![endif]-->
<!--[if gt IE 8]> <!--> <html class="" lang="<?= $language->language ?>" dir="<?= $language->dir ?>"> <!--<![endif]-->
  <head>
    <?= $head ?>
    <title><?= $head_title ?></title>
    <meta name="viewport" content="width=device-width" />
    <meta content="yes" name="apple-mobile-web-app-capable" />
    <meta content="black-translucent" name="apple-mobile-web-app-status-bar-style" />
    <meta content="IE=edge,chrome=1" http-equiv="X-UA-Compatible" />
    <?= $styles ?>
    <?= $scripts ?>
    <!-- IE Fix for HTML5 Tags -->
    <!--[if lt IE 9]>
      <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
      <script src="<?= path_to_theme() ?>/ie.js"></script>
    <![endif]-->
  </head>

  <body class="container <?= $body_classes ?>">
    <div id="top" class="row">
      <div id="skip-link" class="twelvecol last">
        <a href="#main" class="element-invisible element-focusable"><?= t('Skip to main content') ?></a>
      </div>

      <header id="header" role="banner" class="twelvecol last">
        <? if ($logo): ?>
          <a href="<?= $front_page ?>" title="<?= t('Home') ?>" id="logo">
            <img src="<?= $logo ?>" alt="<?= t('Home') ?>" />
          </a>
        <? endif; ?>
        <? if ($site_name || $site_slogan): ?>
          <hgroup id="site-name-slogan">
            <? if ($site_name): ?>
              <h1 id="site-name">
                <a href="<?= $front_page ?>" title="<?= t('Home') ?>"><span><?= $site_name ?></span></a>
              </h1>
            <? endif; ?>
            <? if ($site_slogan): ?>
              <h2 id="site-slogan"><?= $site_slogan ?></h2>
            <? endif; ?>
          </hgroup>
        <? endif; ?>
        <?= $header ?>
      </header>
    </div>

    <div id="middle" class="row">
      <section id="main" role="main" class="twelvecol last">
        <? if (!empty($messages)): ?><?= $messages ?><? endif; ?>
        <? if (!empty($title)): ?><h1 class="title" id="page-title"><?= $title ?></h1><? endif ?>
        <? if (!empty($tabs)): ?><div class="tabs-wrapper"><?= $tabs ?></div><? endif; ?>
        <? if (!empty($help)): ?><?= $help ?><? endif; ?>
        <?= $content ?>
      </section>
    </div>

    <?= $closure ?>
  </body>
</html>