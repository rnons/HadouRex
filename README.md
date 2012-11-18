# HadouRex

HadouRex is a douban.fm cli client written in haskell. I'm rather new to haskell, much of the code are based on or inspired by [hackage: hmp3](http://hackage.haskell.org/package/hmp3). The recommendation mechanism is based on [exaile-doubanfm-plugin](https://github.com/sunng87/exaile-doubanfm-plugin).

## How-To

<pre>
runhaskell dourex.hs
</pre>

You may want to compile it.

<pre>
ghc --make dourex.hs -threaded
</pre>

## Commands

<pre>
./dourex
./dourex listen cid
./dourex search keywords
./dourex hot
./dourex trending
</pre>

When playing, press `h` to see a full list of available controls.

## Dependencies

[hackage: HTTP](http://hackage.haskell.org/package/HTTP-4000.2.6)  
[hackage: json](http://hackage.haskell.org/package/json-0.7)  
[hackage: ansi-terminal](http://hackage.haskell.org/packages/archive/ansi-terminal)
[mpg123](http://www.mpg123.de/)  

## Documentation

[The Making of HadouRex](https://github.com/rnons/HadouRex/wiki/The-Making-of-HadouRex)
