<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="signin">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <div class="control-group">
      <dfLabel class="control-label" ref="loginName"><i18n name="email"/></dfLabel>
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

    <div class="form-actions">
        <dfInputSubmit class="btn btn-large"><i18n name="login"/></dfInputSubmit>
    </div>

</dfForm>
</div>

</apply>