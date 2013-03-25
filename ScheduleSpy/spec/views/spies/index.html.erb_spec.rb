require 'spec_helper'

describe "spies/index.html.erb" do
  before(:each) do
    assign(:spies, [
      stub_model(Spy,
        :email => "Email",
        :department => "Department",
        :course => "Course",
        :section => "Section"
      ),
      stub_model(Spy,
        :email => "Email",
        :department => "Department",
        :course => "Course",
        :section => "Section"
      )
    ])
  end

  it "renders a list of spies" do
    render
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "tr>td", :text => "Email".to_s, :count => 2
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "tr>td", :text => "Department".to_s, :count => 2
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "tr>td", :text => "Course".to_s, :count => 2
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "tr>td", :text => "Section".to_s, :count => 2
  end
end
