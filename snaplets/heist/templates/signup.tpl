<apply template="layout">

<div class="registration">
<form class="form-horizontal" action="signup" method="POST">

    <div class="control-group">
      <label class="control-label" for="input01"><i18n name="email"/></label>
      <div class="controls">
        <input type="text" class="input-large" id="input01">
      </div>
    </div>
    <div class="control-group">
      <label class="control-label" for="input01"><i18n name="repeatEmail"/></label>
      <div class="controls">
        <input type="text" class="input-large" id="input01">
      </div>
    </div>

    <div class="control-group">
      <label class="control-label" for="input01"><i18n name="password"/></label>
      <div class="controls">
        <input type="password" class="input-large" id="input02">
      </div>
    </div>

    <div class="control-group">
      <label class="control-label" for="input01"><i18n name="repeatPassword"/></label>
      <div class="controls">
        <input type="password" class="input-large" id="input02">
      </div>
    </div>

    <div class="form-actions">
        <button type="submit" class="btn btn-large"><i18n name="login"/></button>
    </div>

</form>
</div>

</apply>