#!/bin/bash

## Add capability to run on port 80

sudo setcap 'cap_net_bind_service=+ep' "$1"
