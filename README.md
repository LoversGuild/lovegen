# Generate love, not war!

This repository contains the source code for the Lovers' Guild's static website generator library **LoveGen**.

LoveGen is licensed under the terms of The Affero General Public License version 3.0.

LoveGen has been written to generate the website of Lovers' Guild.
The sources for the site can be found on GitHub [here][web-repo].

[web-repo]: https://github.com/LoversGuild/websites

LoveGen is more or less a general-purpose static site generator.
Its defining feature is support for hierarchical menus in generated pages.

LoveGen is a library which provides simple routines for building websites.
In order to use it, one needs to create a wrapper executable that calls to LoveGen.
For an example, see Lovers' Guild's website repository.

LoveGen is writen in [Haskell][haskell].
It uses the famous [Pandoc][pandoc] library to convert content in Markdown format to HTML.
LoveGen also uses Pandoc's tempaltes.
This is not because Pandoc's templates are good, but because they were easy to configure.
We may well change our template language in the future.

[haskell]: https://haskell.org/
[pandoc]: https://hackage.haskell.org/package/pandoc

# Building

To build LoveGen, you need to have GHC >=9.4 installed.
You also need cabal-install >=3.8.
The best way to install these tools is probably with [GHCup][ghcup].

[ghcup]: https://www.haskell.org/ghcup/

You probably don't want to build lovegen separately but as part of your own site generator.
See Lovers' Guild's [website repository][web-repo] for details.

# Hacking

If you want to make changes to LoveGen, you might need the [precabal tool][precabal-repo] for regenerating LoveGen's cabal file.
This autogeneration is done with the included `autogen` script.

[precabal-repo]: https://github.com/Merivuokko/precabal

Every definition in the generator source code has been documented.
This makes it easier to work with the generator.

The generation logic itself is not yet documented though.
Take a look at the Lovers' Guild's [website repository][web-repo] and see how everything is done.
After all, LoveGen's behaviour is pretty intuitive and does not differ radically from other static site generators.
