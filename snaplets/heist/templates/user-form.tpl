<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="userput/user">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <div class="control-group">
      <dfLabel class="control-label" ref="userEmail">>Email Name</dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="userEmail" required />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="userDisplayName">>displayName</dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="userDisplayName" />
      </div>
    </div>

    <div class="form-actions">
        <i18n name="user-submit">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
    </div>

</dfForm>
</div>

</apply>
