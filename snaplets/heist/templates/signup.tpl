<apply template="layout">

<ifLoggedOut>

<div class="registration">
<dfForm class="form-horizontal" action="signup">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <div class="control-group">
      <dfLabel class="control-label" ref="loginName"><i18n name="username"/></dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="loginName" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="password"><i18n name="password"/></dfLabel>
      <div class="controls">
        <dfInputPassword class="input-large" ref="password" />
      </div>
    </div>

     <div class="control-group">
      <label class="control-label" ref="repeatPassword"><i18n name="repeatPassword"/></label>
      <div class="controls">
        <dfInputPassword class="input-large" ref="repeatPassword" />
      </div>
    </div>

    <div class="form-actions">
        <dfInputSubmit class="btn btn-large"><i18n name="signup"/></dfInputSubmit>
    </div>

</dfForm>
</div>

</ifLoggedOut>


</apply>