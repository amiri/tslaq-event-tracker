#
# Cookbook:: tslaq-event-tracker
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

apt_package "libpq-dev"
apt_package "libpq5"
apt_package "jq"
apt_package "software-properties-common"

app_dir = "/var/local/tslaq-event-tracker"

group "tslaq" do
  gid    2016
end

user "tslaq" do
  comment "TSLAQ User"
  uid 2016
  gid 2016
  home "/home/tslaq"
  shell "/bin/bash"
end

directory "#{app_dir}/bin" do
  owner "tslaq"
  group "tslaq"
  mode "0755"
  recursive true
  action :create
end

directory "#{app_dir}/logs" do
  owner "tslaq"
  group "tslaq"
  mode "0755"
  recursive true
  action :create
end

directory "#{app_dir}/etc/passwords" do
  owner "tslaq"
  group "tslaq"
  mode "0755"
  recursive true
  action :create
  notifies :create_if_missing, "remote_file[#{app_dir}/etc/certs/rds-combined-ca-bundle.pem]", :immediately
end

directory "#{app_dir}/etc/certs" do
  owner "tslaq"
  group "tslaq"
  mode "0755"
  recursive true
  action :create
  notifies :create_if_missing, "remote_file[#{app_dir}/etc/certs/rds-combined-ca-bundle.pem]", :immediately
end

remote_file "#{app_dir}/etc/passwords/basic_auth" do
  source "file:///tmp/deployments/tslaq-event-tracker/etc/passwords/basic_auth"
  owner "tslaq"
  group "tslaq"
  mode "0644"
  subscribes :create_if_missing, "directory[#{app_dir}/etc/passwords]", :immediately
end

file '#{app_dir}/logs/api.log' do
  mode 0755
  owner 'syslog'
  group 'tslaq'
end

remote_file "#{app_dir}/etc/certs/rds-combined-ca-bundle.pem" do
  source "https://s3.amazonaws.com/rds-downloads/rds-combined-ca-bundle.pem"
  owner "tslaq"
  group "tslaq"
  mode "0444"
  subscribes :create_if_missing, "directory[#{app_dir}/etc/certs]", :immediately
end

remote_file "#{app_dir}/bin/tslaq-event-tracker" do
  source "file:///tmp/deployments/tslaq-event-tracker/tslaq-event-tracker-exe"
  owner "tslaq"
  group "tslaq"
  mode "0755"
  action :create
end

execute "copy-react-code" do
  command "rsync -ua --delete /tmp/deployments/tslaq-event-tracker/react/ #{app_dir}/react"
  notifies :run, "execute[chown-react-code]", :immediately
end

execute "chown-react-code" do
  command "chown -Rf tslaq:tslaq #{app_dir}/react && chmod -Rf 0755 #{app_dir}/react"
  subscribes :run, "execute[copy-react-code]", :immediately
end

cookbook_file "/etc/rsyslog.d/tslaq-event-tracker-api.conf"
  source 'tslaq-event-tracker-api.conf'
  owner 'root'
  group 'root'
  mode '0755'
  action :create
end


systemd_unit "tslaq-event-tracker-api.service" do
  content <<-EOM.gsub(/^\s+/, "")
  [Unit]
  Description=Backend servant/WAI server
  After=network.target

  [Service]
  User=tslaq
  WorkingDirectory=#{app_dir}
  ExecStart=#{app_dir}/bin/tslaq-event-tracker
  StandardOutput=syslog
  StandardError=syslog
  SyslogIdentifier=tslaq-event-tracker-api
  Restart=always

  [Install]
  WantedBy=multi-user.target
  EOM

  action [:create, :enable, :reload_or_try_restart]
end
