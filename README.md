# `Fry` - a functional reactive rhythm library

*This library is in an experimental state.*

`fry` delivers a set of operators for defining rhythms 
using *functional reactive programming* (FRP). 
FRP is really good for expressing time-semantics in a declarative way, 
so using it to build rhythmic machines is 
pretty obvious, though I havn't yet seen examples of this. 
The rhythms can control anything you like - for example 
[SuperCollider](https://supercollider.github.io/) via 
[OSC](https://en.wikipedia.org/wiki/Open_Sound_Control). 

`fry` lets you define rhythms statically, but also enables
creation of complex and dynamic rhythmic machinery;
which includes making drum-machines or generative rhythms. You 
add your own inputs and forms of interaction.

A goal of `fry` is to both cater towards experimental artists - 
but also let FRP shine via creating operators that exploit the existing 
declarative FRP semantics in interesting ways.
This is based on the belief that systems for construction should influence the output,
by being used to create the most elegant things possible with the available 
interface - which contrasts with a method of product-design, where 
programming is just 'a means to an end'.

In the `examples` directory you can see how `fry` can be used.

## Using `fry` in your own project

Currently `fry` is not released to `opam` yet, but you can `git clone` 
this repository into a subdir of your own
project that also uses `dune` (or make a `fry` symlink pointing at the
git clone somewhere else)  - then `dune` 
will find the `fry` library when you depend on it in your `dune` file.
See `examples/beat/dune` for an example.

## Compiling

Clone repo:
```bash
git clone https://github.com/rand00/fry.git
cd fry
```

Install dependencies for `fry` and for running the examples:
```bash
opam install lwt lwt_react containers gg notty
```
..note that the `fry` library by itself only depends on `lwt`, `lwt_react` and `containers`,
so it's compatible with e.g. MirageOS.

Compile examples and the library:
```bash
dune build
```

## Running examples:

You pass the path of the examples `main.ml`, with `ml` replaced by `exe`, 
to `dune exec`:
```bash
dune exec examples/ratchet_002/main.exe
```

.. note that the example is recompiled before execution this way, so you can
play around with the examples and see the results with this single command.
