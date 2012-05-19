<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="topic">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorList ref=""></dfChildErrorList>
      </div>
    </dfIfChildErrors>
    <loginErrors />

    <div class="control-group">
      <dfLabel class="control-label" ref="title"><i18n name="topic-title"/></dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="title" />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="content"><i18n name="topic-content"/></dfLabel>
      <div class="controls">
        <dfInputTextArea class="input-large" ref="content" />
      </div>
    </div>

    <div class="form-actions">
        <i18n name="topic-submit">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
    </div>

</dfForm>
</div>

</apply>
