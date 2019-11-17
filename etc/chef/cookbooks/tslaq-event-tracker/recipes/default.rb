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
  filePath = `find /tmp/deployments/tslaq-event-tracker/.stack-work/install -name "tslaq-event-tracker-exe"`
  source "file://#{filePath.chomp()}"
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  action :create
end

execute 'copy-react-code' do
  command 'rsync -ua --delete /tmp/deployments/tslaq-event-tracker/react /var/local/tslaq-event-tracker/'
  notifies :run, 'execute[chown-react-code]', :immediately
end

execute 'chown-react-code' do
  command 'chown -Rf tslaq:tslaq /var/local/tslaq-event-tracker/react && chmod -Rf 0755 /var/local/tslaq-event-tracker/react'
  subscribes :run, 'execute[copy-react-code]', :immediately
end

systemd_unit 'tslaq-event-tracker-api.service' do
  content <<-EOM.gsub(/^\s+/, '')
  [Unit]
  Description=Runs the backend WAIT server
  After=network.target

  [Service]
  User=tslaq
  WorkingDirectory=/var/local/tslaq-event-tracker
  ExecStart=/var/local/tslaq-event-tracker/bin/tslaq-event-tracker
  Restart=always
  Type=forking

  [Install]
  WantedBy=multi-user.target
  EOM

  action [:create, :enable]

end
