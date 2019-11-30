#
# Cookbook:: nginx_wrapper
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

nginx_install 'default' do
    source 'repo'
    default_site_enabled false
    group 'tslaq'
    conf_cookbook 'nginx_wrapper'
    template 'nginx.conf.erb'
    conf_variables(
        nginx_log_dir: '/var/local/tslaq-event-tracker/logs'
    )
end

nginx_site 'default' do
    site_name 'tslaq-event-tracker.org'
    cookbook 'nginx_wrapper'
    template 'tslaq-event-tracker.org.erb'
    variables(
        backend_api_url: 'http://localhost:8888',
        server_root: '/var/local/tslaq-event-tracker/react'
    )
end
