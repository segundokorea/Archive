require 'spec_helper'

describe "spies/show.html.erb" do
  before(:each) do
    @spy = assign(:spy, stub_model(Spy,
      :email => "Email",
      :department => "Department",
      :course => "Course",
      :section => "Section"
    ))
  end

  it "renders attributes in <p>" do
    render
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    rendered.should match(/Email/)
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    rendered.should match(/Department/)
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    rendered.should match(/Course/)
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    rendered.should match(/Section/)
  end
end
