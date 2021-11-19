{% wire id="adlib-import"
        type="submit"
        postback=`import_all`
        delegate=`mod_adlib`
%}
<form method="post" action="postback" id="adlib-import">
<div class="form-group form-inline">
    <select class="form-control" name="endpoint">
        {% for e in m.adlib_api.endpoints %}
          <option value="{{ e.name|escape }}">
              {{ e.name|escape }}  ({{ e.database|escape }})
          </option>
        {% endfor %}
    </select>
    <button class="btn btn-default" type="submit">{_ Import Adlib database _}</button>
    <span class="help-block">
      {_ Select an Adlib database to import all documents from. _}<br>
      {_ The import will be running in the background, progress is reported in the _}:
      <a href="{% url admin_log type="info" message="Adlib" %}">{_ Message log _}</a>
    </span>
</div>
</form>
