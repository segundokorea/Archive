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
        <? if ($primary_links): ?>
          <a href="#navigation" class="element-invisible element-focusable"><?= t('Skip to navigation') ?></a>
        <? endif; ?>
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

        <? if ($search_box): ?><?= $search_box ?><? endif ?>

        <? if ($primary_links || $secondary_links || !empty($navigation)): ?>
          <nav id="navigation" role="navigation">
            <? if (!empty($navigation)): ?> <!--if block in $navigation region, override $primary_links and $secondary_links-->
              <?= $navigation ?>
            <? endif; ?>
            <? if (empty($navigation)): ?> 
              <? if (isset($primary_links)) : ?>
              <?= theme(array('links__system_main_menu', 'links'), $primary_links,
                  array(
                    'id' => 'main-menu',
                    'class' => 'links',
                  ),
                  array(
                    'text' => t('Main menu'),
                    'level' => 'h2',
                    'class' => 'element-invisible',
                  ));
                ?>
              <? endif; ?>
              <? if (isset($secondary_links)) : ?>
              <?= theme(array('links__system_secondary_menu', 'links'), $secondary_links,
                  array(
                    'id' => 'secondary-menu',
                    'class' => 'links',
                  ),
                  array(
                    'text' => t('Secondary menu'),
                    'level' => 'h2',
                    'class' => 'element-invisible',
                  ));
                ?>
              <? endif; ?>
            <? endif; ?>
          </nav> <!-- /#navigation -->
        <? endif; ?>
        <? if (!empty($breadcrumb)): print $breadcrumb; endif; ?>
      </header>
    </div>

    <div id="middle" class="row">

      <? if (!empty($left)): ?>
        <aside id="sidebar-left" role="complementary" class="sidebar threecol">
          <?= $left ?>
        </aside>
      <? endif; ?>

      <?
        $cols = 'twelvecol';
        $last = empty($right) ? 'last' : '';
        if (!empty($left) || !empty($right)) $cols = 'ninecol';
        if (!empty($left) && !empty($right)) $cols = 'sixcol';
      ?>
      <section id="main" role="main" class="<?= $cols ?> <?= $last ?>">
        <? if (!empty($messages)): ?><?= $messages ?><? endif; ?>
        <? if (!empty($mission)): ?><div id="mission"><?= $mission ?></div><? endif; ?>
        <? if (!empty($title)): ?><h1 class="title" id="page-title"><?= $title ?></h1><? endif ?>
        <? if (!empty($tabs)): ?><div class="tabs-wrapper"><?= $tabs ?></div><? endif; ?>
        <? if (!empty($help)): ?><?= $help ?><? endif; ?>
        <?= $content ?>
      </section>

      <? if (!empty($right)): ?>
        <aside id="sidebar-right" role="complementary" class="sidebar threecol last">
          <?= $right ?>
        </aside>
      <? endif; ?>
    </div>

    <? if (!empty($footer)): ?>
      <div id="bottom" class="row">
        <footer id="footer" role="contentinfo" class="twelvecol last">
           <?= $footer ?>
        </footer>
      </div>
    <? endif; ?>

    <?= $closure ?>
  </body>
</html>