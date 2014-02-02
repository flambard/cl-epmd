cl-epmd
=======

Version: 0.2.0

cl-epmd is a Common Lisp library including an EPMD (Erlang Port Mapper Daemon)
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
