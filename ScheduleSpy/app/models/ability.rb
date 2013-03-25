class Ability
  include CanCan::Ability

  def initialize(user)
    user ||= User.new

    can :manage, :all
    can :manage, [:posts] if user.has_role? 'author'
    can :manage, :all if user.has_role? 'admin'
  end
end
