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

      <div class="form-actions">
        <i18n name="user.signup">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
      </div>
    </fieldset>

</dfForm>
</div>

</ifLoggedOut>


</apply>
