function heightUpdateFunction(editor, aceid) {
  // http://stackoverflow.com/questions/11584061/
  var newHeight = Math.max(
    5 * editor.renderer.lineHeight,
    editor.getSession().getScreenLength() * editor.renderer.lineHeight +
      editor.renderer.scrollBar.getWidth()
  );

  $("#" + aceid).height(newHeight.toString() + "px");
  $("#" + aceid + "-section").height(newHeight.toString() + "px");

  // This call is required for the editor to fix all of
  // its inner structure for adapting to a change in size
  editor.resize();
}

function prettify(codeAndParser) {
  var prettyCode,
    error = null;
  try {
    prettyCode = prettier.format(codeAndParser.code, {
      parser: codeAndParser.parser,
      plugins: prettierPlugins,
    });
  } catch (err) {
    error = err.message;
    prettyCode = "";
  }
  Shiny.setInputValue("prettifyError", error, { priority: "event" });
  Shiny.setInputValue("prettyCode", prettyCode, { priority: "event" });
  navigator.clipboard.writeText(prettyCode);
}

function flashFunction(opts) {
  $.alert(opts.message, {
    title: opts.title || null,
    type: opts.type || "info",
    icon: opts.icon || false,
    withTime: opts.withTime || false,
    autoClose: opts.autoClose === false ? false : true,
    closeTime: opts.closeTime || 5000,
    animation: opts.animation || true,
    animShow: opts.animShow || "rotateInDownRight",
    animHide: opts.animHide || "bounceOutLeft",
    position: opts.position || ["bottom-right", [0, 0.01]],
    speed: "slow",
  });
}

$(document).on("shiny:connected", function () {
  Shiny.addCustomMessageHandler("flash", flashFunction);
  var editor = ace.edit("ace");
  heightUpdateFunction(editor, "ace");
  editor.getSession().on("change", function () {
    heightUpdateFunction(editor, "ace");
  });
  var editor_code = ace.edit("code");
  heightUpdateFunction(editor_code, "code");
  editor_code.getSession().on("change", function () {
    heightUpdateFunction(editor_code, "code");
  });
  Shiny.addCustomMessageHandler("code", prettify);
});
