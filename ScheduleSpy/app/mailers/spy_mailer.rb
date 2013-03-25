class SpyMailer < ActionMailer::Base
  default :from => "notifications@schedulespy.com"

  def requires_consent spy
    @spy = spy
    mail(:to => spy.email, :subject => "Consent required for #{spy.department}-#{spy.course} (#{spy.section}) [#{TERM}]")
  end

  def found_opening spy
    @spy = spy
    mail(:to => spy.email, :subject => "Found an opening in #{spy.department}-#{spy.course} (#{spy.section}) [#{TERM}]")
  end

  def does_not_exist spy
    @spy = spy
    mail(:to => spy.email, :subject => "Could not find #{spy.department}-#{spy.course} (#{spy.section}) [#{TERM}]")
  end

  def no_limit spy
    @spy = spy
    mail(:to => spy.email, :subject => "No enrollment limit on #{spy.department}-#{spy.course} (#{spy.section}) [#{TERM}]")
  end
end