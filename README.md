# Backend for llreve

To build, install
[Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
and then run `stack build` in the project directory.

This will build the `llreve-backend` executable and put it somewhere
under `.stack-work` (you should see the location at the end of `stack
build`). Note that this executable links dynamically against a
(relatively small) number of libraries in particular `libgmp` and
`glibc` so the usual caveats apply when it comes to copying this
executable to another machine. (Haskell dependencies are not linked
dynamically).

If you run the `llreve-backend` executable it will open an HTTP server
on port `8080`. `llreve-backend` assumes that `llreve`,
`llreve-dynamic`, `z3` and `eld` (Eldarica) are in PATH.
