#
# Cookbook:: tslaq-event-tracker
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

apt_package 'libpq-dev'
apt_package 'libpq5'
apt_package 'jq'
apt_package 'software-properties-common'
apt_package 'certbot'
apt_package 'python-certbot-nginx'

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

directory '/var/local/tslaq-event-tracker/react/src' do
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

directory '/var/local/tslaq-event-tracker/etc/certs' do
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  recursive true
  action :create
  notifies :create_if_missing, 'remote_file[/var/local/tslaq-event-tracker/etc/certs/rds-combined-ca-bundle.pem]', :immediately
end

remote_file '/var/local/tslaq-event-tracker/etc/certs/rds-combined-ca-bundle.pem' do
  source "https://s3.amazonaws.com/rds-downloads/rds-combined-ca-bundle.pem"
  owner 'tslaq'
  group 'tslaq'
  mode '0444'
  subscribes :create_if_missing, 'directory[/var/local/tslaq-event-tracker/etc/certs]', :immediately
end

remote_file '/var/local/tslaq-event-tracker/bin/tslaq-event-tracker' do
  source 'file:///tmp/deployments/tslaq-event-tracker/tslaq-event-tracker-exe'
  owner 'tslaq'
  group 'tslaq'
  mode '0755'
  action :create
end

execute 'copy-react-code' do
  command 'rsync -ua --delete /tmp/deployments/tslaq-event-tracker/react/ /var/local/tslaq-event-tracker/react'
  notifies :run, 'execute[chown-react-code]', :immediately
end

execute 'chown-react-code' do
  command 'chown -Rf tslaq:tslaq /var/local/tslaq-event-tracker/react && chmod -Rf 0755 /var/local/tslaq-event-tracker/react'
  subscribes :run, 'execute[copy-react-code]', :immediately
end

systemd_unit 'tslaq-event-tracker-api.service' do
  content <<-EOM.gsub(/^\s+/, '')
  [Unit]
  Description=Backend servant/WAI server
  After=network.target

  [Service]
  User=tslaq
  WorkingDirectory=/var/local/tslaq-event-tracker
  ExecStart=/var/local/tslaq-event-tracker/bin/tslaq-event-tracker
  StandardOutput=/var/local/tslaq-event-tracker/logs/api.log
  StandardError=/var/local/tslaq-event-tracker/logs/api.log
  SyslogIdentifier=tslaq-event-tracker-api
  Restart=always

  [Install]
  WantedBy=multi-user.target
  EOM

  action [:create, :enable, :reload_or_try_restart]

end
