require 'spec_helper'

describe "spies/new.html.erb" do
  before(:each) do
    assign(:spy, stub_model(Spy,
      :email => "MyString",
      :department => "MyString",
      :course => "MyString",
      :section => "MyString"
    ).as_new_record)
  end

  it "renders new spy form" do
    render

    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "form", :action => spies_path, :method => "post" do
      assert_select "input#spy_email", :name => "spy[email]"
      assert_select "input#spy_department", :name => "spy[department]"
      assert_select "input#spy_course", :name => "spy[course]"
      assert_select "input#spy_section", :name => "spy[section]"
    end
  end
end
