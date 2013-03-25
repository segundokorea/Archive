class PostsController < ApplicationController
  respond_to :html, :json
  load_and_authorize_resource

  # GET /users
  def index
    respond_with @posts.map( &:attributes )
  end

  # GET /users/new
  def new
    respond_with @post.attributes
  end

  # POST /users
  def create
    @post.author = current_user
    if @post.save
      flash[:notice] = "Successfully created post."
      respond_with( @post.attributes, :status => :created, :location => @post )
    else
      respond_with( @post.errors, :status => :unprocessable_entity ) do |format|
        format.html do
          render :action => :new
        end
      end
    end
  end

  # GET /users/:id
  def show
    respond_with @post.attributes
  end

  # GET /users/:id/edit
  def edit
    respond_with @post.attributes
  end

  # POST /users/:id
  def update
    if @post.update_attributes( params[:post] )
      flash[:notice] = "Successfully updated post."
      respond_with( @post.attributes, :status => :created, :location => @post )
    else
      respond_with( @post.errors, :status => :unprocessable_entity ) do |format|
        format.html do
          render :action => :new
        end
      end
    end
  end

  def destroy
    @post.destroy
    flash[:notice] = "Successfully destroyed post."
    respond_with @post.attributes, :location => posts_path
  end
end
