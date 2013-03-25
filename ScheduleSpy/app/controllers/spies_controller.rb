class SpiesController < ApplicationController
  respond_to :html, :json
  load_and_authorize_resource

  # GET /spies/new
  def new
    respond_with @spy.attributes
  end

  # POST /spies
  def create
    if @spy.save
      cookies[:last_product_id] = @spy.id
      flash[:notice] = "Agent No. #{@spy.id} has accepted the mission"
      respond_with( @spy.attributes, :status => :created, :location => :root )
    else
      respond_with( @spy.errors, :status => :unprocessable_entity ) do |format|
        format.html do
          render :action => :new
        end
      end
    end
  end
end
