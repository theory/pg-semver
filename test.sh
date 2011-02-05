#!/bin/sh

tmpdir=${tmpdir-`pwd`/tmp}
pgbin=/usr/lib/postgresql/9.0/bin

make USE_PGXS=0 || exit 1

[ -f $tmpdir/postmaster.pid ] && PGDATA=$tmpdir $pgbin/pg_ctl stop -m immediate >/dev/null 2>&1
[ -d $tmpdir ] && rm -rf $tmpdir
mkdir $tmpdir
$pgbin/initdb -D $tmpdir

# make the current dir the $libdir
dirname=$(cd `dirname $0`; pwd)
echo "dynamic_library_path='$dirname/src'" >> $tmpdir/postgresql.conf
cat <<EOF >> $tmpdir/postgresql.conf
listen_addresses=''
fsync=no
shared_preload_libraries='\$libdir/auto_explain.so'

custom_variable_classes = 'auto_explain'
auto_explain.log_min_duration = '3s'
EOF

$pgbin/postgres -D $tmpdir -k /tmp 2>$tmpdir/server.err &

