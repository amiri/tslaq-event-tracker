#!/bin/bash

DEBIAN_FRONTEND=noninteractive apt-get -y update
DEBIAN_FRONTEND=noninteractive apt-get -y autoremove
DEBIAN_FRONTEND=noninteractive apt-get -y --allow-unauthenticated --allow-downgrades --allow-remove-essential --allow-change-held-packages -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" dist-upgrade
DEBIAN_FRONTEND=noninteractive apt-get install -y --allow-unauthenticated --allow-downgrades --allow-remove-essential --allow-change-held-packages -o Dpkg::Options::="--force-confold" -o DPkg::Options::="--force-confdef" chef
