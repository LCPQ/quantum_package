#!/bin/bash
#
# Installs curl for ocaml
# Thu Oct 23 22:02:08 CEST 2014

CURL_URL="http://curl.haxx.se/download/curl-7.38.0.tar.gz"

CURL=$(which curl)
if [[ -z ${CURL} ]] 
then 
    rm -f -- bin/curl
    ${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${CURL_URL} CURL.tar.gz 
    tar -zxf CURL.tar.gz && rm CURL.tar.gz ||exit 1
    cd curl* || exit 1 
    cp src/tool_getparam.c src/tool_getparam.c.old
    patch -f < ../data/curl_insecure_patch.txt
    ./configure && make || exit 1 
    ln -s ${PWD}/src/curl ${QPACKAGE_ROOT}/bin 
else
    ln -s ${CURL} ${QPACKAGE_ROOT}/bin/curl
fi

