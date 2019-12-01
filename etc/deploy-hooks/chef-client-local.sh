#!/bin/bash

cd /tmp/deployments/tslaq-event-tracker/etc/chef
chef-client -z -c client.rb -j default-chef.json
# rm -rf /tmp/deployments/*
