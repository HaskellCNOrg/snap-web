<apply template="layout">

  <!--
    1. user basic information
    2. user favorites
  -->
  
<ifFound>
    
    <isCurrentUserAdmin>
        <p>
            <b><i18n name="user.email" />:</b>
            <userEmail/>
        </p>
    </isCurrentUserAdmin>

    <p>
        <b><i18n name="user.displayName"/>:</b>
        <userDisplayName/>
    </p>
    
    <p>
        <b><i18n name="user.siteUrl" />:</b>
        <userSite/>
    </p>
    
    <userLastLoginAt>
        <p>
        <b><i18n name="user.lastLogin" />: </b><lastLoginTime/></p>
    </userLastLoginAt>
    <p>
        <b><i18n name="user.createdSince" />: </b><userCreatedAt/>
    </p>
    
    <userEditable>
        <p><a href="/userput"><i18n name="site.edit" /></a></p>
    </userEditable>
    
  </ifFound>

</apply>
