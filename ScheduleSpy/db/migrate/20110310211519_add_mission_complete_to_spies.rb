class AddMissionCompleteToSpies < ActiveRecord::Migration
  def self.up
    add_column :spies, :mission_complete, :boolean, :default => false
  end
end
