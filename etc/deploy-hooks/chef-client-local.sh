#!/bin/bash

cd /tmp/deployments/tslaq-event-tracker/etc/chef
ls -lastr /tmp/deployments/tslaq-event-tracker/react/
chef-client -z -c client.rb -j default-chef.json
ls -lastr /tmp/deployments/tslaq-event-tracker/react/
ls -lastr /var/local/tslaq-event-tracker/react/
rm -rf /tmp/deployments/*
