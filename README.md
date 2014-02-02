cl-epmd
=======

Version: 0.2.0

cl-epmd is a Common Lisp library including an
[EPMD](http://www.erlang.org/doc/man/epmd.html) (Erlang Port Mapper Daemon)
client and server.


How to install
--------------

Use [Quicklisp](http://www.quicklisp.org/) to install cl-epmd.

    > (ql:quickload :epmd)

### Dependencies

- [binary-data](https://github.com/gigamonkey/monkeylib-binary-data)
- [usocket](http://common-lisp.net/projects/usocket/)

Optional dependencies:

- [FiveAM](http://common-lisp.net/project/fiveam/) (unit-tests)
- [FLEXI-STREAMS](http://weitz.de/flexi-streams/) (for testing)

### How to run the unit-tests

    > (ql:quickload :epmd-test)
    ...
    > (epmd-test:run-all-tests)


Server API
----------

[Function]  
**start** *host* => *nil*

> Start an EPMD server listening on *host*. This function will not return until
> the server is stopped.


Client API
----------

[Function]  
**lookup-node** *node-name `&optional` host* => *node-info*

> Requests node information and distribution port for the node *node-name*
> registered at the EPMD server running at *host*.
>
> The default value of *host* is `"localhost"`.
>
> *lookup-node* returns a *node-info* object if a node with the requested name
> is registered on the server, otherwise *nil* is returned.

[Function]  
**print-all-registered-nodes** *`&optional` host stream* => *t*

> Requests a list of all registered nodes on EPMD server at *host* and prints
> them to *stream*.
>
> The default value of *host* is `"localhost"`.
> The default *stream* is standard output.

[Function]  
**publish** *node-name listening-port* => *epmd-connection*

> Register at the EPMD server running at `localhost`. The node will be
> registered as *node-name* listening to *listening-port*.
>
> *publish* returns an *epmd-connection* object that the application must keep
> as long as it is registered on the server.
