<article class="comment <?= ($comment->new) ? 'comment-new' : '' ?> <?= $status ?> <?= $zebra ?>">
  <header>
    <?= $picture ?>
    <h3><?= $title ?></h3>
    <span class="submitted"><?= $submitted ?></span>
    <? if ($comment->new): ?>
      <span class="new"><?= $new ?></span>
    <? endif; ?>
  </header>
  <div class="content">
    <?= $content ?>
    <? if ($signature): ?>
      <div class="user-signature clearfix">
        <?= $signature ?>
      </div>
    <? endif; ?>
  </div>
  <? if ($links): ?>
    <footer><?= $links ?></footer>
  <? endif; ?>
</article>