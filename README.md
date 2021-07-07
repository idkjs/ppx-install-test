# ppx-install test

The only official ReScript starter template.

## Installation

```sh
npm install
```

## Build

- Build ppx: `npm run build:ppx`
- Build: `npm run build`
- Clean: `npm run clean`
- Build & watch: `node src/FooBindBar.bs.js`


## match%sub is not supported

```sh
rescript: [2/10] src/Test.ast
FAILED: src/Test.ast

  We've found a bug for you!
  /Users/mando/Github/ppx-install-test/src/Test.res:272:7-275:7

  270 ┆ let _arrow_example_1 = (a): X.c<_> =>
  271 ┆   %sub(
  272 ┆     switch a {
  273 ┆     | 0 => return(X.return_v(true))
  274 ┆     | _ => return(X.return_v(false))
  275 ┆     }
  276 ┆   )
  277 ┆

  match%sub is not supported

rescript: [3/10] src/TestRe.ast
FAILED: src/TestRe.ast

  We've found a bug for you!
  /Users/mando/Github/ppx-install-test/lib/bs

  match%sub is not supported

```
