#
# Cookbook:: tslaq-event-tracker
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

apt_package 'libpq-dev'
apt_package 'libpq5'

group 'tslaq' do
  gid    2016
end

user 'tslaq' do
  comment 'TSLAQ User'
  uid 2016
  gid 2016
  home '/home/tslaq'
  shell '/bin/bash'
end

directory '/var/local/tslaq-event-tracker/' do
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  recursive true
  action :create
end

directory '/var/local/tslaq-event-tracker/bin' do
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  recursive true
  action :create
end

remote_file '/var/local/tslaq-event-tracker/bin/tslaq-event-tracker' do
  source 'file:///tmp/deployments/tslaq-event-tracker/.stack-work/install/x86_64-linux/lts-13.6/8.6.3/bin/tslaq-event-tracker-exe'
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  action :create
end

execute 'copy-react-code' do
  command 'rm -rf /var/local/tslaq-event-tracker/react && cp -r /tmp/deployments/tslaq-event-tracker/react /var/local/tslaq-event-tracker/react'
  notifies :run, 'directory[/var/local/tslaq-event-tracker/react]', :immediately
end

directory '/var/local/tslaq-event-tracker/react' do
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  recursive true
  subscribes :run, 'execute[copy-react-code]', :immediately
end
