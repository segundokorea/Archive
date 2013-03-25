class UsersController < ApplicationController
  respond_to :html, :json
  load_and_authorize_resource

  # GET /users
  def index
    respond_with @users.map( &:attributes )
  end

  # GET /users/new
  def new
    respond_with @user.attributes
  end

  # POST /users
  def create
    if @user.save
      flash[:notice] = "User successfully created"
      respond_with( @user.attributes, :status => :created, :location => @user )
    else
      respond_with( @user.errors, :status => :unprocessable_entity ) do |format|
        format.html do
          render :action => :new
        end
      end
    end
  end

  # GET /users/:id
  def show
    respond_with @user.attributes
  end

  # GET /users/:id/edit
  def edit
    respond_with @user.attributes
  end

  # POST /users/:id
  def update
    if @user.update_attributes( params[:user] )
      flash[:notice] = "User successfully updated"
      respond_with( @user.attributes, :status => :updated, :location => @user )
    else
      respond_with( @user.errors, :status => :unprocessable_entity ) do |format|
        format.html do
          render :action => :new
        end
      end
    end
  end
end
