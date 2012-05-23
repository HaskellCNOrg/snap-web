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

  <ifUser>

    <h2>
      <userLogin />
    </h2>
    <p><userLogin/></p>
    <p><userLastLoginAt/></p>
    <p><userCreatedAt/></p>
    <p>
      <userId />
    </p>

  </ifUser>

</apply>
