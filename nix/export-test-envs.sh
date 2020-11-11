#!/bin/sh

export PATH=$PATH:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin

#
# locale
#

export LANG="C.UTF-8"
export LC_ALL="C.UTF-8"

#
# app
#

export RENTIER_ENV="dev"
export RENTIER_LOG_FORMAT="Bracket" # Bracket | JSON
export RENTIER_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/rentier"
export RENTIER_ENDPOINT_PORT="4000"
