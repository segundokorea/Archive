class Post < ActiveRecord::Base
  belongs_to :author, :class_name => 'User', :readonly => true
  attr_accessible :title, :body, :author

  def to_param
    "#{id}-#{title.downcase.gsub( /[^\w\s]+/, '' ).gsub( /\s+/, '-' )}"
  end
end
