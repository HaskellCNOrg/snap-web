## Travis CI

- [![Master](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=master)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)
- [![Branch/0.1](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=branch/0.1)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)

## Impl TODOS

1. [X] dig details of snaplet-auth, valitation checking.
    - directly display error msg from snaplet-auth
2. [X] use mongoDB auth backend rather than JSON
    2.1 [ ] not able to read cfg file. (line 42 `settingsFromConfig`)

3. [X] Be able to do post
4. [X] list posts in home page
5. [X] Be able to view Post detail page.
6. [X] Integrate with Markdown
7. [X] Be able to edit post.
    - [X] 7.1 unfiy those 3 utils 
    - [ ] 7.2 display time per client. check example?
          Seems node-club display time per server.

7. [X] Extra user fileds, e.g. email, homeUrl.
       signup / user detail page.

- [X] Be able to comment
- [ ] FIX: topic author column shall be ObjectId but Text
- [ ] be able to comment to comment

- [ ] authorization, user roles??
- [ ] Styles
- [ ] pagination
- Tag + Category suppert
    - [ ] allow user add tag on the fly when post topic
    - [ ] Category is predifined.

- [ ] Message to user when new comments
- [ ] mail integration (active, reset, etc..)
- [ ] sub folders for tpl?

### Makefile

- [ ] Compress & combine JS
- [ ] Generate min.css per .less files
- [ ] How to update template incorporate those two thing above?

### Snap Technical

- [ ] compress html (remove extra space and comments. )
      paresHTML (seems parseHTML from xmlhtml reads 'return' and extra space as a node??)

## couple of notes
  - rm personal section from sidebar to settings.
    (page probably need to be re-design)
  - show "star person" at sidebar top
  - rm "score board" from sidebar.
  - redesign write post / comment panel
    + preview on the fly?
    + display tag right under input box.
  - customize bootstrap css which is un-necessary large.
  - rm '个性签名' at setting
  - (optional) allow user add tag?
  - (optional) show person score like stackoverflow
  - (optional) mv tags to sidebar.
  - (optional) do not display click count / last update.
  - good examples
    + stackoverflow
    + tumblr.com

## Know bugs
  -  annoy message ("follow myself") when follow people.
  - not display correctly when admin modified post of someone else.
     (It shows modify my author rather than admin)

## Dev question

- The `when.. throw.. or continue` is very impretive. could be more functional?
- DB fatal error when type is `ObjectId` but is text in DB actually.
  (type error is basiacly because how ObjectId implements FromBSON and ToBSON)
