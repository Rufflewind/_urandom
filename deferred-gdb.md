# Deferred GDB

Sometimes you want to debug an executable that is run by another executable.  You may or may not have much control over the way the other executable is being run.  And even if you did, you probably can't start an interactive GDB session from that executable, since it might use the standard I/O streams for internal purposes.

To work around this, one can create a thin script that does little more than to redirect the streams to FIFOs, and then attach to the FIFOs from GDB.  This is easy if the process only cares about stdin.  The script would look like this:

~~~sh
stdin=/tmp/gdb-stdin
rm -f "$stdin"
mkfifo "$stdin" &&
exec cat >"$stdin"
~~~

And you can start a GDB session like this:

~~~sh
gdb /path/to/executable -ex "run </tmp/gdb-stdin"
~~~

It's a little more complicated if you have to redirect multiple streams though.
