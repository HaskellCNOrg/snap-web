## Travis CI

- [![Master](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=master)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)

## Features
### 0.1
 - User Registration
 - Add/Update New Posts
 - Add Comment to either a Topic or Comment
 - Pagination for Posts

## Installation

*Assume OS is \*inux with make otherwise figure out yourself by reading Makefile*

0. Install MongoDB

1. Install Snaplet-Environments

*Need to be in this way because the one in Hackage is not compatibale with snap-0.8*

    git clone git://github.com/HaskellCNOrg/Snaplet-Environments.git
    cd Snaplet-Environments
    make install

2. Install a.haskell.cn

    git clone git://github.com/HaskellCNOrg/a.haskellcn.git
    cp data/env.cfg.default data/env.cfg
    cd a.haskellcn
    make bp

3. Open browser to <http://localhost:9000>

## Production Deployment

0. Assume have done all steps in Installation section

1. cd a.haskellcn

2. make create-site 

*All required files will be copy into _site folder, read Make task for detail*

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


