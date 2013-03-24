<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="/userput">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>

    <div class="control-group">
      <dfLabel class="control-label" ref="userDisplayName"><i18n name="user.displayName" /></dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="userDisplayName" />
      </div>
    </div>

    <div class="control-group">
        <dfLabel class="control-label" ref="userSite"><i18n name="user.siteUrl" /></dfLabel>
        <div class="controls">
          <dfInputText class="input-large" ref="userSite" />
        </div>
    </div>

    <dfInputHidden ref="userVoId" />

    <div class="form-actions">
        <i18n name="user.submit">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
    </div>

</dfForm>
</div>

</apply>
