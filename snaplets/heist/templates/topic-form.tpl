<apply template="layout">

<div class="registration">

<dfForm class="form-horizontal" action="topic">

    <dfIfChildErrors>
      <div class="alert alert-error"> 
         <dfChildErrorListRef />
      </div>
    </dfIfChildErrors>

    <div class="control-group">
      <dfLabel class="control-label" ref="title"><i18n name="topic-title"/></dfLabel>
      <div class="controls">
        <dfInputText class="input-large" ref="title" autofocus />
      </div>
    </div>

    <div class="control-group">
      <dfLabel class="control-label" ref="content" for="wmd-input"><i18n name="topic-content"/></dfLabel>

      <div class="controls">
        <div class="wmd-panel">
          <div id="wmd-button-bar"></div>
          <dfInputTextArea ref="content" class="wmd-input" id="wmd-input" />
        </div>
        <div id="wmd-preview" class="wmd-panel wmd-preview"></div>
      </div>
    </div>

    <dfInputHidden ref="tid" />

    <div class="form-actions">
        <i18n name="topic-submit">
          <dfInputSubmit class="btn btn-large" value="${i18nValue}"></dfInputSubmit>
        </i18n>
    </div>

</dfForm>
</div>

<bind tag="bottom-scripts">
    <apply template="js-markdown" />
    <script type="text/javascript">
      $(function () {
        $('div.alert-error li').each(function (index, x) {
            console.log($(x).attr("data-error"))
            console.log($("[name$=" + $(x).attr("data-error") + "]").closest("div.control-group"))
           $("[name$=" + $(x).attr("data-error") + "]").closest("div.control-group").addClass("error");
        });
      })
    </script>
</bind>

</apply>
