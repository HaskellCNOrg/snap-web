## Travis CI

- [![Master](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=master)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)
- [![Branch/0.1](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=branch/0.1)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)

## Impl TODOS

(@) [X] dig details of snaplet-auth, valitation checking.
    - directly display error msg from snaplet-auth
(@) [X] use mongoDB auth backend rather than JSON
    - [ ] not able to read cfg file. (line 42 `settingsFromConfig`)

(@) [X] Be able to do post
(@) [X] list posts in home page
(@) [X] Be able to view Post detail page.
(@) [X] Integrate with Markdown
(@) [X] Be able to edit post.
    - [X] unfiy those 3 utils 
    - [ ] display time per client. check example?
          Seems node-club display time per server.
(@) [X] Extra user fileds, e.g. email, homeUrl.
        signup / user detail page.
(@) [X] Be able to comment
(@) [X] FIX: Reply author column shall be ObjectId but Text
(@) [X] Markdown does support Chinese!!???

(@) [ ] be able to comment to comment
    - [ ] group comments

- [/] Highlight error on input box
      a fix at private branch.
      
- [ ] Tag allow user add tag on the fly when post topic
- [ ] FIX: topic author column shall be ObjectId but Text

- [ ] pagination
- [ ] Message to user when new comments
- [ ] Styles
- [ ] mail integration (active, reset, etc..)

- [ ] authorization, user roles??
    - [ ] just config to file like node_club
    - [ ] database integration.

- [ ] Category is predifined.
- [ ] sub folders for tpl?

### Makefile

- [ ] Compress & combine JS
- [ ] Generate min.css per .less files
- [ ] How to update template incorporate those two thing above?

### Snap Technical

(@) compress html
    - paresHTML (seems parseHTML from xmlhtml reads 'return' and extra space as a node??)
    - to make thing simply, just remove extra space/line break and comments.

## couple of notes
- [X] rm personal section from sidebar to settings.
      (page probably need to be re-design)
- [X] show "star person" at sidebar top
- [X] rm "score board" from sidebar.
- redesign write post / comment panel
    - [X] preview on the fly?
    - [ ] display tag right under input box.
- [ ] customize bootstrap css which is un-necessary large.
- [ ] allow user add tag?
- [ ] show person score like stackoverflow
- [ ] mv tags to sidebar.
- [ ] do not display click count.

## Dev question

- The `when.. throw.. or continue` is very impretive. could be more functional?
- DB fatal error when type is `ObjectId` but is text in DB actually.
  (type error is basiacly because how ObjectId implements FromBSON and ToBSON)
