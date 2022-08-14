# MAL implementation in ADA

This is yet another Ada implementation of MAL lisp dialect.
I have done it to learn Ada language and to satisfy my inner
necesity to write compilers/interpreters.
See [https://github.com/kanaka/mal](https://github.com/kanaka/mal) for the details.

It passes all tests, including self-hosting ones.

## Build

Because it initially resided inside [https://github.com/kanaka/mal](https://github.com/kanaka/mal),
the building process is not so straight with `alr` or `gnatmake`, use `make`:

```
make bin/stepA_mal
```
This builds the latest version of the interpreter.

To build with alire directly, `STEP` environment variable must be set to one of the proper values,
see Makefile for details.
