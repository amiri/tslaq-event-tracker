resource_name :api_executable
property :filepath, String, default: ''

action :run do
    puts `find /tmp/deployments/tslaq-event-tracker/.stack-work/install -name "tslaq-event-tracker-exe"`
end



