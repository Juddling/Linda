# Linda in Erlang

This is a computer science project written by James Judd while studying at the University of York.

## How-to
To compile the source code make sure you're in the correct directory and run:
```
cd /c/git/linda
erl -make
```

To run bring up an Erlang instance including the BEAM files for this library use:

```
erl -pa ebin/
```

To connect multiple instances you must start two or more Erlang instances (in separate terminals) with different names
and the same secret cookie:

```
erl -pa ebin/ -sname x -cookie SECRET_LINDA_COOKIE
erl -pa ebin/ -sname y -cookie SECRET_LINDA_COOKIE
```

To connect the two instances:

```
net_adm:ping('x@your_system').
```