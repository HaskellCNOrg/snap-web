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

      <div class="control-group">
        <label class="control-label"><i18n name="user-email" /></label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userEmail/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label"><i18n name="user-display-name" /></label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userDisplayName/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label"><i18n name="user-site-url" /></label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userSite/></span>
        </div>
      </div>

    </form>

    
    <p><i18n name="user-last-login" />: <userLastLoginAt/></p>
    <p><i18n name="user-created-since" />: <userCreatedAt/></p>

    <p>
      <a href="/userput"><i18n name="site-edit" /></a>
    </p>

  </ifFound>

</apply>
