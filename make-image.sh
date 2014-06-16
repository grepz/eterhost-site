#!/bin/sh

buildapp \
    --asdf-tree ~/quicklisp/ \
    --output ~/tmp/eterhost \
    --load-system eterhost-site \
    --entry eterhost-site::main
