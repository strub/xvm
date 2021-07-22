// --------------------------------------------------------------------
var asm = "\
; Compute the factorial of the top element of the stack\n\
\n\
	PUSH 10             ; Factorial input\n\
fact:\n\
	FETCH 0\n\
	PUSH 0\n\
	WRITE\n\
fact_r:\n\
	PUSH 0\n\
	READ\n\
	PUSH 1\n\
	SUB\n\
	FETCH 0\n\
	GTZ fact_end\n\
	FETCH 0\n\
	PUSH 0\n\
	WRITE\n\
	MUL\n\
	GTO fact_r\n\
fact_end:\n\
	POP\n\
	PRT                  ; Print result\n\
	STOP"

// --------------------------------------------------------------------
function xvmctxt() {
  this.editor = $('#editor');
  this.result = $('#result');
  this.state  = null;

  this.editor = ace.edit("editor");
  this.editor.getSession().setMode("ace/mode/xvm");
  this.editor.setTheme("ace/theme/textmate");
  this.editor.setValue(asm, -1);

  this.editor.session.gutterRenderer = {
    getWidth: function(session, lastLineNumber, config) {
      return (2 + lastLineNumber.toString().length) * config.characterWidth;
    },

    getText: $.proxy(function(session, row) {
      if (this.state == null)
        return '';
      pc = this.state.revidx(row);
      if (pc < 0)
        return ''
      return "0x" + ("0" + (Number(pc).toString(16))).slice(-2).toUpperCase();
    }, this),
  };

  this.result = ace.edit("result");
  this.result.getSession().setMode("ace/mode/text");
  this.result.setTheme("ace/theme/textmate");
  this.result.setReadOnly(true);
  this.result.setOptions({ fontSize: "8pt" });
  this.result.renderer.setShowGutter(false);

  this.terminal = ace.edit("terminal");
  this.terminal.getSession().setMode("ace/mode/text");
  this.terminal.setTheme("ace/theme/textmate");
  this.terminal.setReadOnly(true);
  this.terminal.setOptions({ fontSize: "8pt" });
  this.terminal.renderer.setShowGutter(false);

  this.msg = $('#msg');
  this.marker = null;

  this.lazyinit = $.proxy(function() {
    if (this.state == null) {
      this.editor.setValue(this.editor.getValue(), -1);
      this.state = xvm.create(this.print, this.editor.getValue());
    }
  }, this);

  this.clearpc = $.proxy(function() {
    if (this.marker != null)
      this.editor.session.removeMarker(this.marker);
    this.marker = null;
  }, this);

  this.clearmsg = $.proxy(function() {
    this.msg.text('');
    this.msg.hide();
  }, this);

  this.setmsg = $.proxy(function(msg) {
    this.msg.text(msg);
    this.msg.show();
  }, this);

  this.clear = $.proxy(function() {
    this.result.setValue('', -1);
    this.clearpc();
  }, this);    

  this.print = $.proxy(function(value) {
    this.terminal.session.insert({
      row: this.terminal.session.getLength(), column: 0
    }, value + '\n');
  }, this)

  this.dump = $.proxy(function() {
    this.clear();

    if (this.state != null) {
      var rg = ace.require('ace/range').Range;
      var pc = this.state.pc();

      pc = new rg(pc, 0, pc, 1);
      this.result.setValue(this.state.dump(), -1);
      this.marker = this.editor.session.addMarker(pc, "pc-marker", "fullLine");
      this.editor.$blockScrolling = Infinity;
      if (this.state.pc() <  this.editor.getFirstVisibleRow() + 1 ||
          this.state.pc() >= this.editor.getLastVisibleRow () - 1 )
      {
        this.editor.scrollToLine(this.state.pc());
      }
    }
  }, this);

  this.reset = $.proxy(function() {
    this.clearmsg();
    try {
      this.state = null;
      this.lazyinit();
    } catch (e) {
      this.setmsg(e.message);
    }
    this.terminal.setValue('', -1);
    this.dump();
  }, this);

  this.run = $.proxy(function(until) {
    this.clearmsg();
    try {
      this.lazyinit();
      this.state.run(until);
    } catch (e) {
      this.setmsg(e.message);
    }
    this.dump();
  }, this);

  this.step = $.proxy(function() {
    try {
      this.clearmsg();
      this.lazyinit();
      this.state.step();
    } catch (e) {
      this.setmsg(e.message);
    }
    this.dump();
  }, this);

  $('#run').click($.proxy(function() {
    this.run(128);
    this.editor.focus();
  }, this));

  $('#step').click($.proxy(function() {
    this.step();
    this.editor.focus();
  }, this));

  $('#reset').click($.proxy(function() {
    this.reset();
    this.editor.focus();
  }, this));

  this.editor.on("change", $.proxy(function() {
    this.clear();
    this.state = null;
  }, this));
}

// --------------------------------------------------------------------
var myxvm = null;

$(document).ready(function() {
  myxvm = new xvmctxt();
  myxvm.editor.focus();
  myxvm.reset();
});
