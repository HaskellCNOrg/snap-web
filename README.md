online demo: <http://demo.haskellcn.org>

## Travis CI

- [![Master](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=master)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)

## Features
### 0.1
 - User Registration
 - Add/Update New Posts
 - Add Comment to either a Topic or Comment
 - Pagination for Posts
 - i18n support

### 0.2

## Installation

**Assume OS is \*inux with make otherwise figure out yourself by reading Makefile**

0. Install MongoDB
   
1. Install a.haskell.cn

```
git clone git://github.com/HaskellCNOrg/a.haskellcn.git
cd a.haskellcn
cp data/env.cfg.default data/env.cfg
make init bp
```

2. Open browser to <http://localhost:9900>

## Production Deployment

0. Assume have done all steps in Installation section

1. cd a.haskellcn

2. make create-site 

*All required files will be copy into _site folder, read make task for detail*

## Notes

1. Customization files

- `data/env.cfg`, which has config that not going to be shared
- `data/main.cfg`
- `data/message-*.cfg`

2. Is it better to specify version of dependency rather than range?

## License

Check the LICENSE file

## Contribute

Feel free ask questiones and contribute.


