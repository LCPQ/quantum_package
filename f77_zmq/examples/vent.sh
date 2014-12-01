#!/bin/bash

source shell_functions.sh

function ventclient ()
{
  for i in {1..4}
  do
    ./ventclient &
  done
  wait
}

function ventserver ()
{
  (sleep 2 ; echo) | ./ventserver
}

SERVER=ventserver
CLIENT=ventclient 

run
