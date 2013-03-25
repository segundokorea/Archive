# Framework 3.x for Drupal 6
#### Created by André Griffin
#### Responsive fork by Sean Clemmer

For the original, [check out the module on Drupal.org](http://drupal.org/project/framework).

Framework is a blank canvas for theme developers. Use Framework as a user friendly starting point to help facilitate your theme development. Build site themes without feeling like you have to reinvent the wheel or remove unnecessary code every time.

Framework is actively developed and supported by André Griffin. If you would like to say thanks, please consider [donating via PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=1532730).

## Usage

Install to `sites/all/themes`.

To fully convert Framework to your own custom theme, it is encouraged that you: 

- Copy the `framework` directory within `sites/all/themes/` and rename it `YOUR_THEME_NAME`
- Change the filename and contents of the `.info` file accordingly, and remove all "packing script" info at the end of the file
- Use find/replace in `template.php` and `template.block-editing.inc` to replace `framework` with `YOUR_THEME_NAME`
- Upload and enable the theme

To create a sub-theme based on Framework, [check out this article on creating sub-themes](http://drupal.org/node/225125).