<apply template="layout">

    <ifFound>
        
        <userEditable>
            <p>
                <b><i18n name="user.email" />:</b>
                <userEmail/>
            </p>
        </userEditable>
        
        <p>
            <b><i18n name="user.displayName"/>:</b>
            <userDisplayName/>
        </p>
        
        <p>
            <b><i18n name="user.siteUrl" />:</b>
            <userSite />
        </p>
        
        <isAuthUser>
            <p>
                <b><i18n name="user.createdSince" />: </b><createdAt/>
            </p>
            <p>
                <b><i18n name="user.lastLogin" />: </b><lastLoginTime/>
            </p>
        </isAuthUser>
        
        <userEditable>
            <p><a href="/userput/${userId}"><i18n name="site.edit" /></a></p>
        </userEditable>
        
    </ifFound>
    
</apply>
