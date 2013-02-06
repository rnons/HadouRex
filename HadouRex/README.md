# HadouRex

HadouRex is a [douban.fm] cli client written in haskell. Some code are based on or inspired by [hackage: hmp3](http://hackage.haskell.org/package/hmp3). The recommendation mechanism is based on [exaile-doubanfm-plugin](https://github.com/sunng87/exaile-doubanfm-plugin).

**Note**: HadouRex now depends on [doubanfm.hs], which is a [douban.fm] wrapper library.

## How-To

**Install [doubanfm.hs] first!**

### To run

<pre>
runhaskell dourex.hs
</pre>

### To build

<pre>
ghc --make dourex.hs -threaded
</pre>

### To install

<pre>
cabal install
</pre>

## Commands

<pre>
dourex
dourex listen [cid/artist]
dourex search keywords
dourex hot
dourex trending
dourex mark cid
dourex unmark cid
dourex marks
</pre>

In `dourex listen arg`, if arg is all integer, dourex will assume it to be *channel id*. Otherwise, it will be assumed as *artist name*.  

Note: You need to quote the *artist name* when it consists of more than one word. e.g.

`dourex listen "sigur ros"`

When playing, press `h` to see a full list of available controls.

## Dependencies

- see `HadouRex.cabal`
- [mpg123](http://www.mpg123.de/)  

## Documentation

[The Making of HadouRex](https://github.com/rnons/HadouRex/wiki/The-Making-of-HadouRex)

[douban.fm]: http://douban.fm
[doubanfm.hs]: https://github.com/rnons/doubanfm.hs
