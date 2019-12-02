#
# Cookbook:: nginx_wrapper
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

app_dir = "/var/local/tslaq-event-tracker"

nginx_install "default" do
    source "repo"
    default_site_enabled false
    group "tslaq"
    conf_cookbook "nginx_wrapper"
    conf_template "nginx.conf.erb"
    conf_variables(
        nginx_log_dir: "#{app_dir}/logs/",
        app_dir: "#{app_dir}",
    )
end

nginx_site "default" do
    site_name "tslaq-event-tracker.org"
    cookbook "nginx_wrapper"
    template "tslaq-event-tracker.org.erb"
    variables(
        backend_api_url: "127.0.0.1:8888",
        server_root: "#{app_dir}/react/",
    )
end

service 'nginx' do
  extend Nginx::Cookbook::Helpers
  supports restart: true, status: true, reload: true
  action :nothing
end

openssl_dhparam "#{app_dir}/etc/certs/dhparam.pem" do
  owner "nginx"
  group "nginx"
  key_length 2048
  notifies :restart, "service[nginx]"
end

site = "tslaq-event-tracker.org"
sans = ["www.#{site}"]

acme_selfsigned "#{site}" do
  crt     "#{app_dir}/etc/certs/#{site}.crt"
  key     "#{app_dir}/etc/certs/#{site}.key"
  chain    "#{app_dir}/etc/certs/#{site}.pem"
  owner   "nginx"
  group   "nginx"
  notifies :restart, "service[nginx]", :immediate
end

# Get and auto-renew the certificate from Let"s Encrypt
acme_certificate "#{site}" do
  crt     "#{app_dir}/etc/certs/#{site}.crt"
  key     "#{app_dir}/etc/certs/#{site}.key"
  wwwroot           "#{app_dir}/react"
  notifies :restart, "service[nginx]"
  alt_names sans
end
