<apply template="layout">

<ifLoggedOut>
<div class="registration">

<dfForm class="form-horizontal" action="/signin">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <fieldset>
      <legend><i18n name="user.signin" /></legend>

      <div class="control-group">
        <dfLabel class="control-label" ref="loginName"><i18n name="user.userName"/></dfLabel>
        <div class="controls">
          <dfInputText class="input-large" ref="loginName" type="email" required autofocus placeholder="me@example.com" />
        </div>
      </div>
      
      <div class="control-group">
        <dfLabel class="control-label" ref="password"><i18n name="user.password"/></dfLabel>
        <div class="controls">
          <dfInputPassword class="input-large" ref="password" required />
        </div>
      </div>
      
      <div class="form-actions">
        <i18n name="user.login">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
      </div>
    </fieldset>

</dfForm>
</div>
</ifLoggedOut>

<ifLoggedIn>
  <p>You already signin as <loggedInUser/></p>
</ifLoggedIn>

</apply>
