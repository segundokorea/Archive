ActionMailer::Base.delivery_method = :smtp

if ENV['RAILS_ENV'] == "production"
  ActionMailer::Base.smtp_settings = {
    :address => '127.0.0.1',
    :port    => 25,
    :domain  => 'localhost'
  }
else
  ActionMailer::Base.smtp_settings = {
    :address              => 'smtp.gmail.com',
    :port                 => 587,
    :domain               => 'www.schedulespy.com',
    :authentication       => :plain,
    :user_name            => 'paul@schedulespy.com',
    :password             => 'seaman01',
    :enable_starttls_auto => true
  }
end