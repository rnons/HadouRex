## 库

**doubanfm.hs**是doubanfm api的haskell封装。目前还不具备登录功能。

    cd doubanfm.hs
    cabal install

## 播放器

**HadouRex**是doubanfm的CLI客户端，后端调用[mpg123]进行播放。因此，除了cabal install还需要另外安装mpg123。 

HadouRex的代码参考了[hmp3]和[exaile-doubanfm-plugin]。

### 安装

    cd HadouRex
    cabal install

名为 **dourex** 的可执行文件会被安装到 *.~/.cabal/bin* 。

### 选项

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

dourex的选项模仿了包管理器。用`dourex hot`和`dourex trending`查看热门和上升最快的兆赫。用`dourex search`搜索兆赫。 

**注意**: dourex listen 后面接的参数，既可以是电台id，也可以是歌手/乐队名。如

    dourex listen 1002316
    dourex listen "sigur ros"

dourex播放时，可以按`h`键查看快捷键。

## 几个可以改进的地方

1. 增加登录功能，可以参考[fmd]
2. 作为一种练习，用[aeson]代替[json]库进行JSON的解析。aeson的口碑似乎比较好
3. 作为一种练习，用[http-conduit]代替[HTTP]进行http请求
4. 用[mpd]代替mpg123进行音乐播放
5. 用`putMVar`和`getMVar`机制，代替`mpg123wait`函数，可以参考[jinkell]
6. 增加歌词，可以考虑[geci.me]
7. 用[optparse-applicative]进行选项(listen，hot等)的解析。

这几个方面我或多或少知道怎么去实现，不过暂时没有时间去做。 

有兴趣的同学可以进行尝试，重要的是有兴趣，不会的地方可以找我探讨(irc #haskell-cn)。  

或者如果你想到其它可以改进的地方，也欢迎告诉我。


----
[hmp3]: http://hackage.haskell.org/package/hmp3
[mpg123]: http://www.mpg123.de/
[aeson]: http://hackage.haskell.org/package/aeson
[json]: http://hackage.haskell.org/package/json
[http-conduit]: hackage.haskell.org/package/http-conduit
[HTTP]: http://hackage.haskell.org/package/HTTP
[fmd]: https://github.com/hzqtc/fmd
[jinkell]: https://github.com/rnons/jinkell
[geci.me]: https://github.com/solos/geci.me-api
[exaile-doubanfm-plugin]: https://github.com/sunng87/exaile-doubanfm-plugin
[optparse-applicative]: http://hackage.haskell.org/package/optparse-applicative
