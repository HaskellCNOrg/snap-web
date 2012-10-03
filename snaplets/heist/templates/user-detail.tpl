<apply template="layout">

  <!--
    1. user basic information
    2. user favorites
  -->
  
<!--   <ifUserError>
    <div class="alert alert-error"> 
      <ul>
        <li><exceptionValue/></li>
      </ul>
    </div>
    <returnToHome />
  </ifUserError> -->

  <ifFound>

    <form class="form-horizontal">

      <fieldset>
        <legend><userDisplayName/></legend>

      <div class="control-group">
        <label class="control-label"><i18n name="user.email" /></label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userEmail/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label"><i18n name="user.displayName" /></label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userDisplayName/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label"><i18n name="user.siteUrl" /></label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userSite/></span>
        </div>
      </div>

      </fieldset>

    </form>

    
    <userLastLoginAt><p><i18n name="user.lastLogin" />: <lastLoginTime/></p></userLastLoginAt>
    <p><i18n name="user.createdSince" />: <userCreatedAt/></p>

    <userEditable>
    <p><a href="/userput"><i18n name="site.edit" /></a></p>
    </userEditable>

  </ifFound>

</apply>
