# Haskell bindings to bsdiff

[bsdiff][] is a tool for building and applying patches to binary
software.

These are the awesome Haskell bindings, based on [minibsdiff][], a slimmed down
version of bsdiff v4.3.

This package also includes a reimplementation of @bsdiff/bspatch@, called
@bspatcher@, which demonstrates how to use the library and offers several
compression modes as opposed to just bzlib like the standard bsdiff. If you
want to enable snappy support in @bspatcher@, install with the @-fsnappy@ flag.

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
[minibsdiff]: http://github.com/thoughtpolice/minibsdiff
[main page]: http://thoughtpolice.github.com/hs-bsdiff
[issue tracker]: http://github.com/thoughtpolice/hs-bsdiff/issues
[gh]: http://github.com/thoughtpolice/hs-bsdiff
[bb]: http://bitbucket.org/thoughtpolice/hs-bsdiff
[Hackage]: http://hackage.haskell.org/package/bsdiff
