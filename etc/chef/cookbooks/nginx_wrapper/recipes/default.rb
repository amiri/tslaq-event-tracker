#
# Cookbook:: nginx_wrapper
# Recipe:: default
#
# Copyright:: 2019, The Authors, All Rights Reserved.

nginx_install 'default' do
    source 'repo'
    default_site_enabled false
    group 'tslaq'
    conf_variables(
        nginx_log_dir: '/var/local/tslaq-event-tracker/logs'
    )
end
