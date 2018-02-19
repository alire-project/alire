#!/bin/bash

gprclean -q -r -Palr_env

{
    find . -path '**src**/*.ads' -o -path '**src**/*.adb' | xargs basename -a
    gprbuild -j0 -p -q -Palr_env -bargs -R | grep -e '\.ad.$' | xargs -n1
} | sort | uniq -u | grep -v alire-index | grep -v demo | grep -v alire-project.ads | grep -v project_skel
