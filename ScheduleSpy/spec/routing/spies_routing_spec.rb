require "spec_helper"

describe SpiesController do
  describe "routing" do

    it "recognizes and generates #index" do
      { :get => "/spies" }.should route_to(:controller => "spies", :action => "index")
    end

    it "recognizes and generates #new" do
      { :get => "/spies/new" }.should route_to(:controller => "spies", :action => "new")
    end

    it "recognizes and generates #show" do
      { :get => "/spies/1" }.should route_to(:controller => "spies", :action => "show", :id => "1")
    end

    it "recognizes and generates #edit" do
      { :get => "/spies/1/edit" }.should route_to(:controller => "spies", :action => "edit", :id => "1")
    end

    it "recognizes and generates #create" do
      { :post => "/spies" }.should route_to(:controller => "spies", :action => "create")
    end

    it "recognizes and generates #update" do
      { :put => "/spies/1" }.should route_to(:controller => "spies", :action => "update", :id => "1")
    end

    it "recognizes and generates #destroy" do
      { :delete => "/spies/1" }.should route_to(:controller => "spies", :action => "destroy", :id => "1")
    end

  end
end
