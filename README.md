# Ile

Ile is a work-in-progress compiled, statically typed functional language
that uses the Go runtime.

It aims to scratch an itch of mine in what I see as a gap in the general-purpose programming
language space. I think there is space for a language with:

- A type system as expressive as Typescript's, with its union and literal types, which are pleasant to use for domain
  modelling
- A runtime as well-designed as Go's, where
    - static binaries free us from worrying about where the software is going to run
    - abstracted system calls allow us to use non-blocking IO easily, without
      having to think about whether we are blocking the underlying thread
- An opinionated lifecycle for data structures, so that it does not involve zero-values (like in C and Go)
  and instead forces us to think about how we initialise data.
- Fast prototyping capabilities so that you can write safe code without fighting the type system

More generally, the language aims to be good at:

- Domain modelling (thanks to union and intersection types)
- Being foolproof by avoiding some common footguns (like nil pointers or zero values) in favour of explicit
  initialisation and
  null-checking type-safety
- Powerful-but-safe concurrency, via goroutines and enforced structured concurrency
- Do the above while being pleasant and productive, via flow-typing and type inference

The novel, exciting part is combining principal type inference (ie, you don't need to write any types) with
proper union types. This is achieved with a type-system primarily based
on [MLStruct (2022, Parreaux et al.)](https://2022.splashcon.org/details/splash-2022-oopsla/41/MLstruct-Principal-Type-Inference-in-a-Boolean-Algebra-of-Structural-Types).

I am still unsure to what extent the language should leverage principal type inference,
and types that are part of a library's interface will probably still need them.

