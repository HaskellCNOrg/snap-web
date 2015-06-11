<apply template="layout">

    <bind tag="subtitle"> :: <i18n name="page.forgotpassword"/></bind>

<ifLoggedOut>
    <dfForm class="form-horizontal" action="/forgotPassword">

        <dfIfChildErrors>
            <div class="alert alert-error"> 
                <dfChildErrorList ref=""></dfChildErrorList>
            </div>
        </dfIfChildErrors>
        
        <loginErrors />
        
        <fieldset>
            
            <legend><i18n name="user.resetPassword" /></legend>
            
            <apply template="_simple-signup-fields" />
            
            <div class="form-actions">
                <i18n name="user.resetPassword">
                    <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
                </i18n>
            </div>
            
        </fieldset>
        
    </dfForm>
    
</ifLoggedOut>

</apply>
