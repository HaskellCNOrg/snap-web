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

    <h2>
      <userLogin />
    </h2>

    <form class="form-horizontal">
      <div class="control-group">
        <label class="control-label">User Name</label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userLogin/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label">Display Name</label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userDisplayName/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label">User Email</label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userEmail/></span>
        </div>
      </div>

      <div class="control-group">
        <label class="control-label">User Site</label>
        <div class="controls">
          <span class="input-xlarge uneditable-input"><userSite/></span>
        </div>
      </div>

    </form>

    
    <p>Last login: <userLastLoginAt/></p>
    <p>User created since: <userCreatedAt/></p>

    <p>
      <a href="/userput">Edit</a>
    </p>

  </ifFound>

</apply>
