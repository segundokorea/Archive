class CreateSpies < ActiveRecord::Migration
  def self.up
    create_table :spies do |t|
      t.string :email
      t.string :department
      t.string :course
      t.string :section

      t.timestamps
    end
  end

  def self.down
    drop_table :spies
  end
end
