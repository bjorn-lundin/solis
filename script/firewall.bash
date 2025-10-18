#!/bin/bash


function add_port () {
  port=$1
  comment=$2
  echo "adding port ${port} for service ${comment}" 
  sudo firewall-cmd --zone=public --add-port=${port}/tcp --permanent
}


function add_service () {
  service=$1
  echo "adding service ${service}" 
  sudo firewall-cmd --zone=public --add-service=${service} --permanent

}

add_port 9080 http-local
add_port 5432 pgsql

add_service http
add_service https
add_service ssh

sudo firewall-cmd --reload
sudo firewall-cmd --list-all


