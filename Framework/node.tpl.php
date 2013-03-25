<? if (!$page): ?>
  <article id="node-<?= $node->nid ?>" class="node <?= ($sticky) ? 'sticky' : '' ?> <?= (!$status) ? 'node-unpublished' : '' ?>">
<? endif; ?>

  <? if ($picture || $submitted || !$page): ?>
    <? if (!$page): ?><header><? endif; ?>

      <? if ($picture): ?>
        <?= $picture ?>
      <? endif; ?>

      <? if (!$page): ?>
        <h2><a href="<?= $node_url ?>" title="<?= $title ?>"><?= $title ?></a></h2>
      <? endif; ?>

      <? if ($submitted): ?>
        <span class="submitted"><?= $submitted ?></span>
      <? endif; ?>

    <? if (!$page): ?></header><? endif; ?>
  <? endif; ?>

  <div class="content">
    <?= $content ?>
  </div>

  <? if (!empty($terms) || !empty($links)): ?>
    <footer>
      <? if ($terms): ?>
        <div class="terms">
          <span><?= t('Tags: ') ?></span><?= $terms ?>
        </div>
      <? endif; ?>
      <? if ($links): ?>
        <div class="links">
          <?= $links ?>
        </div>
      <? endif; ?>
    </footer>
  <? endif; ?>

<? if (!$page): ?></article><? endif; ?>