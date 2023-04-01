# `Fry` - a functional reactive rhythm library

*This library is in an experimental state.*

The idea of `fry` is to deliver a set of operators for defining rhythms 
using *functional reactive programming* (FRP). 
FRP is really good for expressing time-semantics in a declarative way, 
so using it to build rhythmic machines is 
pretty obvious, though I havn't yet seen examples of this. 
The rhythms can control anything you like - for example 
[SuperCollider](https://supercollider.github.io/) via 
[OSC](https://en.wikipedia.org/wiki/Open_Sound_Control). 

`fry` not only lets you define statically defined rhythms, but has the 
goal of enabling creation of more complex and dynamic rhythmic machinery;
which includes dynamic changes to rhythms by some form of user interaction
or generative rhythms. 

Another goal is to both cater towards what experimental musicians/artists 
could want as features - but also let FRP shine via creating operators
that exploit existing FRP operators and semantics.
This idea is based on the belief that systems should influence the output,
by being used to create the most elegant things possible with the available 
interfaces - which contrasts with a method of product-design, where 
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

Compiling all examples and the library:
```bash
dune build
```

## Running examples:

You pass the path of the examples `main.ml`, with `ml` replaced by `exe`, 
to `dune exec`:
```bash
dune exec examples/ratchet_002/main.exe
```

