require 'spec_helper'

# This spec was generated by rspec-rails when you ran the scaffold generator.
# It demonstrates how one might use RSpec to specify the controller code that
# was generated by the Rails when you ran the scaffold generator.

describe SpiesController do

  def mock_spy(stubs={})
    @mock_spy ||= mock_model(Spy, stubs).as_null_object
  end

  describe "GET index" do
    it "assigns all spies as @spies" do
      Spy.stub(:all) { [mock_spy] }
      get :index
      assigns(:spies).should eq([mock_spy])
    end
  end

  describe "GET show" do
    it "assigns the requested spy as @spy" do
      Spy.stub(:find).with("37") { mock_spy }
      get :show, :id => "37"
      assigns(:spy).should be(mock_spy)
    end
  end

  describe "GET new" do
    it "assigns a new spy as @spy" do
      Spy.stub(:new) { mock_spy }
      get :new
      assigns(:spy).should be(mock_spy)
    end
  end

  describe "GET edit" do
    it "assigns the requested spy as @spy" do
      Spy.stub(:find).with("37") { mock_spy }
      get :edit, :id => "37"
      assigns(:spy).should be(mock_spy)
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "assigns a newly created spy as @spy" do
        Spy.stub(:new).with({'these' => 'params'}) { mock_spy(:save => true) }
        post :create, :spy => {'these' => 'params'}
        assigns(:spy).should be(mock_spy)
      end

      it "redirects to the created spy" do
        Spy.stub(:new) { mock_spy(:save => true) }
        post :create, :spy => {}
        response.should redirect_to(spy_url(mock_spy))
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved spy as @spy" do
        Spy.stub(:new).with({'these' => 'params'}) { mock_spy(:save => false) }
        post :create, :spy => {'these' => 'params'}
        assigns(:spy).should be(mock_spy)
      end

      it "re-renders the 'new' template" do
        Spy.stub(:new) { mock_spy(:save => false) }
        post :create, :spy => {}
        response.should render_template("new")
      end
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested spy" do
        Spy.stub(:find).with("37") { mock_spy }
        mock_spy.should_receive(:update_attributes).with({'these' => 'params'})
        put :update, :id => "37", :spy => {'these' => 'params'}
      end

      it "assigns the requested spy as @spy" do
        Spy.stub(:find) { mock_spy(:update_attributes => true) }
        put :update, :id => "1"
        assigns(:spy).should be(mock_spy)
      end

      it "redirects to the spy" do
        Spy.stub(:find) { mock_spy(:update_attributes => true) }
        put :update, :id => "1"
        response.should redirect_to(spy_url(mock_spy))
      end
    end

    describe "with invalid params" do
      it "assigns the spy as @spy" do
        Spy.stub(:find) { mock_spy(:update_attributes => false) }
        put :update, :id => "1"
        assigns(:spy).should be(mock_spy)
      end

      it "re-renders the 'edit' template" do
        Spy.stub(:find) { mock_spy(:update_attributes => false) }
        put :update, :id => "1"
        response.should render_template("edit")
      end
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested spy" do
      Spy.stub(:find).with("37") { mock_spy }
      mock_spy.should_receive(:destroy)
      delete :destroy, :id => "37"
    end

    it "redirects to the spies list" do
      Spy.stub(:find) { mock_spy }
      delete :destroy, :id => "1"
      response.should redirect_to(spies_url)
    end
  end

end
