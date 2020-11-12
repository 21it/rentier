#!/bin/sh

set -m

export PATH=$PATH:/bin/
export PGDATA=$PWD/postgres
export PGHOST=/tmp/postgres
export PGLOG=$PGDATA/LOG
export PGDATABASE=postgres
export DATABASE_URL="postgresql:///postgres?host=$PGHOST"

#
# Postgres
#

if [[ $EUID -ne 0 ]]; then
  alias postgres-sh="sh"
else
  alias postgres-sh="su -m nixbld1"
fi

if [ ! -d $PGHOST ]; then
  echo "initializing postgresql workspace..."
  postgres-sh -c "mkdir -p $PGHOST"
fi

if [ ! -d $PGDATA ]; then
  echo "initializing postgresql database..."
  postgres-sh -c "mkdir -p $PGDATA"
  postgres-sh -c "initdb $PGDATA --encoding=UTF8 --auth=trust > /dev/null"
fi

if [[ $EUID -ne 0 ]]; then
  echo "operating as root..."
  chown -R root $PGDATA
  chown -R root $PGHOST
else
  echo "operating as nixbld1..."
  chown -R nixbld1 $PGDATA
  chown -R nixbld1 $PGHOST
fi

echo "starting postgres..."
postgres-sh -c 'pg_ctl start -o "-c listen_addresses=localhost -c unix_socket_directories=$PGHOST" > /dev/null'

until postgres-sh -c 'pg_isready' 2>/dev/null; do
  >&2 echo "postgres is unavailable - sleeping for 3 seconds..."
  sleep 3
done

postgres-sh -c 'createuser rentier'
postgres-sh -c 'createdb rentier'
postgres-sh -c 'createdb rentier_test'
echo "spawn-test-deps executed"
