<apply template="layout">

<ifLoggedOut>
<div class="registration">

<dfForm class="form-horizontal" action="/signin" name="signin-form">

    <dfIfChildErrors>
      <div class="alert alert-error">
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <fieldset>
      <legend><i18n name="user.signin" /></legend>

      <div class="form-group">
        <dfLabel class="col-sm-2 control-label" ref="loginName"><i18n name="user.userName"/></dfLabel>
        <div class="col-sm-4">
          <dfInputText class="form-control" ref="loginName" type="email" required autofocus placeholder="me@example.com" />
        </div>
      </div>

      <div class="form-group">
        <dfLabel class="col-sm-2 control-label" ref="password"><i18n name="user.password"/></dfLabel>
        <div class="col-sm-4">
          <dfInputPassword class="form-control" ref="password" required />
        </div>
      </div>

      <dfInputHidden ref="nextPageUri" />

      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-4">
        <i18n name="user.login">
          <dfInputSubmit class="btn btn-lg btn-default" value="${i18nValue}"></dfInputSubmit>
        </i18n>
        <a class="btn btn-link" href="/forgotPassword"><i18n name="user.forgotPassword" /></a>
        </div>

      </div>
    </fieldset>

</dfForm>
</div>
</ifLoggedOut>

<ifLoggedIn>
  <p>You already signin as <loggedInUser/></p>
</ifLoggedIn>

</apply>
