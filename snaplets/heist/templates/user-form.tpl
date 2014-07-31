<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="/userput">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>

    <div class="form-group">
      <dfLabel class="col-sm-2 control-label" ref="userDisplayName"><i18n name="user.displayName" /></dfLabel>
      <div class="col-sm-6">
        <dfInputText class="form-control" ref="userDisplayName" />
      </div>
    </div>

    <div class="form-group">
        <dfLabel class="col-sm-2 control-label" ref="userSite"><i18n name="user.siteUrl" /></dfLabel>
        <div class="col-sm-6">
          <dfInputText class="form-control" ref="userSite" />
        </div>
    </div>

    <dfInputHidden ref="userVoId" />

    <div class="form-group">
         <div class="col-sm-offset-2 col-sm-6">
           <i18n name="user.submit">
             <dfInputSubmit class="btn btn-default btn-lg" value="${i18nValue}"></dfInputSubmit>
         </i18n>
        </div>
    </div>

</dfForm>
</div>

</apply>
