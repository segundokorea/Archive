<section id="block-<?= $block->module .'-'. $block->delta ?>" class="block block-<?= $block->module ?>">
  <? if (!empty($block->subject)): ?>
    <h2><?= $block->subject ?></h2>
  <? endif; ?>
  <div class="content">
    <?= $edit_links ?>
    <?= $block->content ?>
  </div>
</section>