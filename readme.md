scala-syntax
[![Travis Build Status](https://travis-ci.org/scalacenter/scala-syntax.svg?branch=master)](https://travis-ci.org/scalacenter/scala-syntax)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/ys1uejcy2y2tgamf/branch/master?svg=true)](https://ci.appveyor.com/project/scalacenter/scala-syntax/branch/master)
[![codecov.io](http://codecov.io/github/scalacenter/scala-syntax/coverage.svg?branch=master)](http://codecov.io/github/scalacenter/scala-syntax?branch=master)
[![Join the chat at https://gitter.im/scalacenter/scalafix](https://badges.gitter.im/scala-syntax.svg)](https://gitter.im/scala-syntax)
========

# Scalameta tree pretty printer

Pretty printer for [Scalameta](http://scalameta.org/) trees using
[Paiges](http://github.com/typelevel/paiges).
Improves the built-in Scalameta pretty printer (`Tree.syntax`) with

* more correct handling of precedence rules, inserting parentheses where necessary.
* line wrapping so that large expressions don't appear in a single line with
  hundreds of columns.
* ability to preserve comments, enabling syntax preserving tree transforms
* better performance

The end goal of this project is to enable a more powerful refactoring API for
[Scalafix](https://scalacenter.github.io/scalafix/).
Currently, Scalafix rewrites are implemented using a fairly low-level token
API, which is error-prone.


## Team

The current maintainers (people who can merge pull requests) are:

- Guillaume Massé - [`@@MasseGuillaume`](https://github.com/@MasseGuillaume)
- Ólafur Páll Geirsson - [`@olafurpg`](https://github.com/olafurpg)


