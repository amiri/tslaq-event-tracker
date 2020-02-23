#
# Cookbook:: acme_wrapper
# Recipe:: default
#
# Copyright:: 2020, The Authors, All Rights Reserved.

include_recipe 'acme::default'

patched_gem_path = '/tmp/acme-client-2.0.5.gem'

cookbook_file "#{patched_gem_path}" do
  source 'acme-client-2.0.5.gem'
  owner 'root'
  group 'root'
  mode '0755'
  action :create
end

edit_resource(:chef_gem, 'acme-client') do
  source "#{patched_gem_path}"
end




