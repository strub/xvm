// --------------------------------------------------------------------
define('ace/mode/xvm', [], function(require, exports, module) {

var oop = require("ace/lib/oop");
var TextMode = require("ace/mode/text").Mode;
var Tokenizer = require("ace/tokenizer").Tokenizer;

var XVMHighlightRules = require("ace/mode/xvm-rules").XVMHighlightRules;

var Mode = function() {
  this.HighlightRules = XVMHighlightRules;
};
oop.inherits(Mode, TextMode);

(function() {
   this.lineCommentStart = ";";
 }).call(Mode.prototype);

exports.Mode = Mode;
});

// --------------------------------------------------------------------
define('ace/mode/xvm-rules', [], function(require, exports, module) {

var oop = require("ace/lib/oop");
var TextHighlightRules = require("ace/mode/text_highlight_rules").TextHighlightRules;

var XVMHighlightRules = function() {
  this.$rules = {
    "start": [
      { token: 'keyword',
        regex: '\\b(?:ADD|SUB|MIN|MUL|DIV|AND|OR|XOR|NOT|EQ|LT|LE|PUSH|READ|WRITE|POP|RET|PXR|PRX|RHP|WHP|ALLOC|FETCH|RFR|WFR|CREAD|CWRITE|GTO|GTZ|GSB|STOP|PRT)\\b' },
      { token: 'entity.name.function.assembly', regex: '^\\s*\\w+\\s*:$' },
      { token: 'constant.numeric', regex: '\\b\\d+\\b' },
      { token: 'comment.line.assembly', regex: ';.*$' },
    ]
  };
  this.normalizeRules()
};

oop.inherits(XVMHighlightRules, TextHighlightRules);

exports.XVMHighlightRules = XVMHighlightRules;
});
