<apply template="layout">

<ifLoggedOut>

<div class="registration">
<dfForm class="form-horizontal" action="/signup">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <fieldset>
      <legend><i18n name="user.signup" /></legend>
      
      <apply template="_simple-signup-fields" />

      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-4">
        <i18n name="user.signup">
          <dfInputSubmit class="btn btn-default btn-lg" value="${i18nValue}"></dfInputSubmit>
        </i18n>
        </div>
      </div>
    </fieldset>

</dfForm>
</div>

</ifLoggedOut>


</apply>
