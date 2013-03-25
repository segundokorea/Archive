# Load the rails application
require File.expand_path('../application', __FILE__)
require 'open-uri'

# Enjoy!
SITE_NAME     = 'ScheduleSpy'
TERM_ARGUMENT = '&term=449'
TERM          = 'Winter 2012'

UPDATE_INTERVAL = 300 # 5 [minutes]
NOWORK_INTERVAL = 30  # [seconds]

# Initialize the rails application
ScheduleSpy::Application.initialize!

Less::More.header = false

# Configure invalid form fields
ActionView::Base.field_error_proc = Proc.new do |html_tag, instance|
  if html_tag =~ /class=/i
    html_tag.gsub!( /class="(.*?)"/i, 'class="#{$1} rejected"' )
  else
    html_tag.sub!( '>', 'class="rejected">' )
  end
  html_tag
end