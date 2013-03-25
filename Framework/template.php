<?php

/**
 * Create a conformant class or id for HTML attributes.
 */
function framework_id_safe($string) {
  // Replace with dashes anything that isn't A-Z, numbers, dashes, or underscores.
  $id = strtolower(preg_replace('/[^a-zA-Z0-9-]+/', '-', $string));

  // If the first character is not a-z, add a prefix.
  // Don't use ctype_alpha since its locale aware.
  if (!ctype_lower($string{0})) $id = 'id-' . $id;

  return $id;
}



/**
 * Return a themed breadcrumb trail.
 *
 * @param $breadcrumb
 *   An array containing the breadcrumb links.
 * @return a string containing the breadcrumb output.
 */
function framework_breadcrumb($breadcrumb) {
  if (empty($breadcrumb)) return;
  // Provide a navigational heading to give context for breadcrumb links to
  // screen-reader users. Make the heading invisible with .element-invisible.
  $heading = '<h2 class="element-invisible">' . t('You are here') . '</h2>';
  $breadcrumb[] = drupal_get_title(); // Add current page to breadcrumbs.
  return '<nav class="breadcrumb">' . $heading . implode(' » ', $breadcrumb) . '</nav>';
}


function framework_links($links, $attributes = array('class' => 'links'), $heading = '') {
  global $language;
  $output = '';

  if (count($links) > 0) {
    // Treat the heading first if it is present to prepend it to the
    // list of links.
    if (!empty($heading)) {
      if (is_string($heading)) {
        // Prepare the array that will be used when the passed heading is a string.
        $heading = array(
          'text' => $heading,
          'level' => 'h2', // Set the default level of the heading.
        );
      }
      $output .= '<' . $heading['level'];
      if (!empty($heading['class'])) {
        $output .= drupal_attributes(array('class' => $heading['class']));
      }
      $output .= '>' . check_plain($heading['text']) . '</' . $heading['level'] . '>';
    }

    $output .= '<ul'. drupal_attributes($attributes) .'>';

    $num_links = count($links);
    $i = 1;

    foreach ($links as $key => $link) {
      $class = $key;

      // Add first, last and active classes to the list of links to help out themers.
      if ($i == 1) $class .= ' first';
      if ($i == $num_links) $class .= ' last';

      if (isset($link['href']) && ($link['href'] == $_GET['q'] || ($link['href'] == '<front>' && drupal_is_front_page()))
          && (empty($link['language']) || $link['language']->language == $language->language)) {
        $class .= ' active';
      }
      $output .= '<li'. drupal_attributes(array('class' => $class)) .'>';

      if (isset($link['href'])) {
        // Pass in $link as $options, they share the same keys.
        $output .= l($link['title'], $link['href'], $link);
      } else if (!empty($link['title'])) {
        // Some links are actually not links, but we wrap these in <span> for adding title and class attributes
        if (empty($link['html'])) {
          $link['title'] = check_plain($link['title']);
        }
        $span_attributes = '';
        if (isset($link['attributes'])) {
          $span_attributes = drupal_attributes($link['attributes']);
        }
        $output .= '<span'. $span_attributes .'>'. $link['title'] .'</span>';
      }

      $output .= "</li>\n";
      $i++;
    }

    $output .= '</ul>';
  }

  return $output;
}



/**
 * Allow themable wrapping of all comments.
 */
function framework_comment_wrapper($content, $node) {
  if (!$content || $node->type == 'forum') {
    return '<section id="comments">'. $content .'</section>';
  }
  return '<section id="comments"><h2>'. t('Comments') .'</h2>'. $content .'</section>';
}



/**
 * Allow theming of publishing information.
 */
function framework_node_submitted($node) {
  return t(
    'Published by !username on !datetime',
    array(
      '!username'     => '<span class="author">'. theme('username', $node). '</span>',
      '!datetime'     => '<time datetime="!fulldatetime" pubdate>'. format_date($node->created). '</time>',
      '!fulldatetime' => format_date($node->created, 'custom', 'Y-m-d\TH:i:sZ')
    )
  );
}


function framework_comment_submitted($comment) {
  return t(
    '!username | !datetime',
    array(
      '!username'     => '<span class="author">'. theme('username', $comment). '</span>',
      '!datetime'     => '<time datetime="!fulldatetime" pubdate>'. format_date($comment->timestamp). '</time>',
      '!fulldatetime' => format_date($comment->created, 'custom', 'Y-m-d\TH:i:sZ')
    )
  );
}



/**
 * Duplicate of theme_menu_local_tasks() but adds clearfix to tabs.
 */
function framework_menu_local_tasks() {
  $output = '';

  // CTools requires a different set of local task functions.
  if (module_exists('ctools')) {
    ctools_include('menu');
    $primary   = ctools_menu_primary_local_tasks();
    $secondary = ctools_menu_secondary_local_tasks();
  } else {
    $primary   = menu_primary_local_tasks();
    $secondary = menu_secondary_local_tasks();
  }

  if ($primary)   $output .= '<ul class="tabs primary clearfix">' . $primary . '</ul>';
  if ($secondary) $output .= '<ul class="tabs secondary clearfix">' . $secondary . '</ul>';
  return $output;
}



/**
 * Override or insert variables into the block templates.
 *
 * @param $vars
 *   An array of variables to pass to the theme template.
 * @param $hook
 *   The name of the template being rendered ("block" in this case.)
 */
function framework_preprocess_block(&$vars, $hook) {
  $block = $vars['block'];

  $vars['content'] = $block->content;
  $vars['title']   = $block->subject;

  // Special classes for blocks.
  $vars['classes_array'][] = 'block-' . $block->module;
  $vars['classes_array'][] = 'region-' . $vars['block_zebra'];
  $vars['classes_array'][] = $vars['zebra'];
  $vars['classes_array'][] = 'region-count-' . $vars['block_id'];
  $vars['classes_array'][] = 'count-' . $vars['id'];

  // Create the block ID.
  $vars['block_html_id'] = 'block-' . $block->module . '-' . $block->delta;

  $vars['edit_links_array'] = array();
  if (user_access('administer blocks')) {
    include_once './' . drupal_get_path('theme', 'framework') . '/template.block-editing.inc';
    framework_preprocess_block_editing($vars, $hook);
    $vars['classes_array'][] = 'with-block-editing';
  }
  $vars['edit_links'] = !empty($vars['edit_links_array']) ? '<div class="edit">' . implode(' ', $vars['edit_links_array']) . '</div>' : '';
}