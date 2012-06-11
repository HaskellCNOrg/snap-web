## Travis CI

- [![Master](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=master)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)
- [![Branch/0.1](https://secure.travis-ci.org/HaskellCNOrg/a.haskellcn.png?branch=branch/0.1)](http://travis-ci.org/HaskellCNOrg/a.haskellcn)

## Implementation

### TODOS

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

(@) [X] be able to comment to comment
    - [X] group comments

- [/] Highlight error on input box.
      a fix at private branch.
      
- [X] FIX: topic author column shall be ObjectId but Text
- [ ] Tag allow user add tag on the fly when post topic

- [ ] Message to user when new comments
- [ ] authorization, user roles??
    - [ ] just config to file like node_club
    - [ ] database integration.

- [ ] pagination
- [ ] Styles
- [ ] mail integration via email-postmark (active, reset, etc..)
- [ ] redirect with 303 rather than 302

- [ ] Category is predifined.
- [ ] sub folders for tpl?
- [ ] JS markdown parse is diff with pandoc.
- [ ] Page shortcuts. JIRA!

- [ ] whether POST action successfully or not, it is likely to finish it with a redirection,
      which means TWO requests to user, isn't it overload??

### Makefile

- [ ] Compress & combine JS
- [ ] Generate min.css per .less files
- [ ] How to update template incorporate those two thing above?

### Know issues

1. When not login, access handlers that run with `withAuthUser` in ajax way.
   Reproduce: allow any user post comment to a comment.


## Dev question

- compress html
    - paresHTML (seems parseHTML from xmlhtml reads 'return' and extra space as a node??)
    - to make thing simply, just remove extra space/line break and comments.

- The `when.. throw.. or continue` is very impretive. could be more functional?

- DB fatal error when type is `ObjectId` but is text in DB actually.
  (type error is basiacly because how ObjectId implements FromBSON and ToBSON)

- error "No handler accepted" when a template not found.
  basically it is because `render` return `empty` when no template found.
    - simple write msg to output when error will cause error because Heist handler all request (withHeist "")

- https://github.com/dbp/heist-async


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

