online demo: <http://demo.haskellcn.org>

## Travis CI

- [![branch-0.2](https://secure.travis-ci.org/HaskellCNOrg/snap-web.png?branch=branch/0.2)](http://travis-ci.org/HaskellCNOrg/snap-web)

## Features
### 0.1
 - User Registration
 - Add/Update New Posts
 - Add Comment to either a Topic or Comment
 - Pagination for Posts
 - i18n support

### 0.2
  - Add tag per topic
  - Literature Haskell
  - XSS

## Installation

**Assume OS is \*inux with make otherwise figure out yourself by reading Makefile**

  0. Install MongoDB
  1. Install snap-web
  
    ```
    git clone git://github.com/HaskellCNOrg/snap-web.git
    cd snap-web
    make init bp
    ```
    
  2. Open browser to <http://localhost:9900>

## Production Deployment

  0. Assume have done all steps in Installation section
  1. install `nodejs`; install `less` and `ugilify-js` package
  2. cd snap-web and `make create-site`
  4. (**Important**) update `_site/prod.cfg` per your env.

*All required files will be copy into _site folder, read make task for detail*

## Notes

1. Customization files
  - `prod.cfg`, `devel.cfg`
  - `data/main.cfg`
  - `data/message-*.cfg`

## License

Check the LICENSE file

## Contribute

Feel free ask questiones and contribute.


