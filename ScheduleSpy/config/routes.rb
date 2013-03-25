ScheduleSpy::Application.routes.draw do
  root :to => "spies#new"

  match 'about'  => 'high_voltage/pages#show', :id => 'about'

  devise_for :users, :path_names => {
    :sign_in      => 'login',
    :sign_out     => 'logout',
    :confirmation => 'verification',
    :sign_up      => 'signup'
  }

  match 'signup' => redirect( '/users/new' )
  match 'login'  => redirect( '/users/login')
  match 'logout' => redirect( '/users/logout')

  resources :blog, :controller => 'posts', :as => 'posts'
  resources :users, :except => [ :delete ]
  resources :spies, :only => [ :new, :create ]
end
