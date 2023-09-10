# Generate love, not war!

This repository contains the source code for the Lovers' Guild's static website generator **LoveGen**.

LoveGen is licensed under the terms of The Affero General Public License version 3.0.

LoveGen has been written to generate the website of Lovers' Guild.
The sources for the site can be found on GitHub [here][web-repo].

[web-repo]: https://github.com/LoversGuild/websites

LoveGen is more or less a general-purpose static site generator.
Its general usefulness is somewhat limited though by the fact that it has a built-in configuration.
This will hopefully change soon.

LoveGen is writen in [Haskell][haskell].
It uses the famous [Pandoc][pandoc] library to convert content in Markdown format to HTML.
LoveGen also uses Pandoc's tempaltes.
This is not because Pandoc's templates are good, but because they were easy to configure.
We may well change our template language in the future.

[haskell]: https://haskell.org/
[pandoc]: https://hackage.haskell.org/package/pandoc

# Building

To build LoveGen, you need to have GHC >=9.4 installed.
You also need cabal-install >=3.8 – no stack build files are provided.
The best way to install these tools is probably with
[GHCup][ghcup].

[ghcup]: https://www.haskell.org/ghcup/

To build LoveGen,, run:

```sh
$ cabal build
```

To install LoveGen to `$HOME/.cabal/bin` (or whatever your Cabal's configured `bin` directory is), run `cabal install`.

# Usage

To run the site generator, execute either of the following commands.

If you installed LoveGen somerwhere where your shell can find it, run:
```sh
$ lovegen -C <website directory>
```
or just
```sh
$ lovegen
```
if `<website directory>` is your working directory.

If you did not install LoveGen, run `cabal run lovegen -- -C <website directory>` in the root directory of LoveGen's sources.

`<website directory>` is the directory where your website contents are stored.

Some configuration settings of LoveGen are statically built-in.
These include all directory names and Pandoc options.
To change LoveGen's configuration, edit `src/Main.hs`.
First look for `defaultSiteConfig`.

When run, LoveGen reads site contents from various directories under the website root directory.
By default these are: `pages/` for page contents, `static/` for static files and `tempaltes/` for page templates.

It then generates the ready-to-deploy site to the `output/` directory.
The build system also stores cache data to the `shake/` directory.

The generator uses `Development.Shake.Forward` module for tracking build dependencies.
This does not work very well yet (probably due to some misconfiguration on our side).
As a result, you may occasionally need to `rm -rf output shake` after editing the site contents.
Sometimes the build fails noisily if the dependency tracking fails.
Sometimes though, you'll just get an incomplete or incorrectly rendered site.

# Hacking

Every definition in the generator source code has been documented.
This makes it easier to work with the generator.

The generation logic itself is not yet documented though.
Take a look at the Lovers' Guild's [website repository][web-repo] and see how everything is done.
After all, LoveGen's behaviour is pretty intuitive and does not differ radically from other static site generators.
