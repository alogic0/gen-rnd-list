#!/bin/bash

## apt-get install libcap2-bin
## Adds capability to run on port 80

sudo setcap 'cap_net_bind_service=+ep' "$1"
