# Haskell bindings to bsdiff

[bsdiff][] is a tool for building and applying patches to binary
software.

These are the awesome Haskell bindings, based on bsdiff v4.3. Also included is
a small reimplementation of `bsdiff/bspatch` called `bspatcher` that
demonstrates how to use the API and uses [lz4](http://code.google.com/p/lz4)
for compression.

The `src/cbits` directory contains a copy of
[minibsdiff](http://github.com/thoughtpolice/minibsdiff) which is the
underlying API. You can reuse it however you like.

[travis-ci.org](http://travis-ci.org) results: [![Build Status](https://secure.travis-ci.org/thoughtpolice/hs-bsdiff.png?branch=master)](http://travis-ci.org/thoughtpolice/hs-bsdiff)

[Homepage][main page].

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install bsdiff
```

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-bsdiff.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-bsdiff.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-bsdiff/master/AUTHORS.txt).

# License

The library is BSD3. However, the `bspatcher` executable is under the GPLv2 (or
later,) the same as [QuickLZ](http://quicklz.com) itself and [my Haskell
binding](http://hackage.haskell.org/package/quicklz) since it uses it by
default. See `LICENSE.txt` for terms of copyright and redistribution.

[bsdiff]: http://www.daemonology.net/bsdiff/
[main page]: http://thoughtpolice.github.com/hs-bsdiff
[issue tracker]: http://github.com/thoughtpolice/hs-bsdiff/issues
[gh]: http://github.com/thoughtpolice/hs-bsdiff
[bb]: http://bitbucket.org/thoughtpolice/hs-bsdiff
[Hackage]: http://hackage.haskell.org/package/bsdiff
