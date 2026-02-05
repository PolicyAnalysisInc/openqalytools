// Formula Input - Single-line R code editor for Shiny
// Uses Ace Editor for syntax highlighting with custom term highlighting

(function() {
  "use strict";

  // Wait for Shiny, Ace, and our custom modules to be available
  function waitForDependencies() {
    return new Promise((resolve) => {
      const check = () => {
        if (typeof Shiny !== "undefined" &&
            Shiny.inputBindings &&
            typeof ace !== "undefined" &&
            typeof FormulaInputMode !== "undefined") {
          resolve();
        } else {
          setTimeout(check, 50);
        }
      };
      check();
    });
  }

  // Validate bracket matching
  function validateBrackets(code) {
    const stack = [];
    const pairs = { "(": ")", "[": "]", "{": "}" };
    const opening = Object.keys(pairs);
    const closing = Object.values(pairs);

    let inString = false;
    let stringChar = null;
    let inComment = false;

    for (let i = 0; i < code.length; i++) {
      const char = code[i];

      if (char === "#" && !inString) {
        inComment = true;
        continue;
      }

      if (inComment) continue;

      if (char === '"' || char === "'") {
        let numBackslashes = 0;
        for (let j = i - 1; j >= 0 && code[j] === '\\'; j--) {
          numBackslashes++;
        }
        const isEscaped = numBackslashes % 2 === 1;

        if (!isEscaped) {
          if (!inString) {
            inString = true;
            stringChar = char;
          } else if (char === stringChar) {
            inString = false;
            stringChar = null;
          }
        }
        continue;
      }

      if (inString) continue;

      if (opening.includes(char)) {
        stack.push(char);
      } else if (closing.includes(char)) {
        const last = stack.pop();
        if (!last || pairs[last] !== char) {
          return false;
        }
      }
    }

    return stack.length === 0;
  }

  // Initialize when dependencies are ready
  waitForDependencies().then(() => {
    // Inject default styles for custom token types
    FormulaInputMode.injectDefaultStyles();

    // Create the Shiny input binding
    const FormulaInputBinding = new Shiny.InputBinding();

    $.extend(FormulaInputBinding, {
      find: function(scope) {
        return $(scope).find(".formula-input");
      },

      initialize: function(el) {
        const initialValue = $(el).data("value") || "";
        const placeholderText = $(el).data("placeholder") || "";
        const termsData = $(el).data("terms");

        // Parse terms if provided
        let initialTerms = null;
        if (termsData) {
          try {
            // jQuery may have already parsed the JSON
            initialTerms = typeof termsData === "string" ? JSON.parse(termsData) : termsData;
          } catch (e) {
            console.warn("Failed to parse formula input terms:", e);
          }
        }

        // Create Ace editor
        const editor = ace.edit(el);
        editor.setTheme("ace/theme/chrome");
        editor.session.setMode("ace/mode/r");

        // Configure for single-line appearance
        editor.setOptions({
          maxLines: 1,
          minLines: 1,
          showGutter: false,
          showPrintMargin: false,
          highlightActiveLine: false,
          showFoldWidgets: false,
          displayIndentGuides: false,
          scrollPastEnd: 0,
          useSoftTabs: true,
          tabSize: 2
        });

        // Set placeholder
        if (placeholderText) {
          editor.setOption("placeholder", placeholderText);
        }

        // Set initial value
        editor.setValue(initialValue, -1);

        // Initialize custom highlighter for term highlighting
        const highlighter = new FormulaInputMode.FormulaHighlighter(editor);
        if (initialTerms) {
          highlighter.setTerms(initialTerms);
        }
        el._formulaHighlighter = highlighter;

        // Intercept Enter key - trigger Shiny update instead of newline
        editor.commands.addCommand({
          name: "submitFormula",
          bindKey: { win: "Enter", mac: "Enter" },
          exec: function() {
            $(el).trigger("formula-input:enter");
          }
        });

        // Also intercept Shift+Enter just in case
        editor.commands.addCommand({
          name: "submitFormulaShift",
          bindKey: { win: "Shift-Enter", mac: "Shift-Enter" },
          exec: function() {
            $(el).trigger("formula-input:enter");
          }
        });

        // Handle paste - strip newlines to keep single-line
        // Use DOM-level event because Ace's paste events are unreliable
        editor.container.addEventListener("paste", function(e) {
          e.preventDefault();
          e.stopPropagation();
          var text = (e.clipboardData || window.clipboardData).getData("text");
          text = text.replace(/[\r\n]+/g, " ");
          editor.insert(text);
        }, true);

        // Trigger change event on input
        editor.on("change", function() {
          $(el).trigger("formula-input:change");
        });

        // Store editor reference
        el._formulaEditor = editor;
      },

      getValue: function(el) {
        const editor = el._formulaEditor;
        if (!editor) return { value: "", valid: true };

        const value = editor.getValue();
        const valid = validateBrackets(value);

        return { value: value, valid: valid };
      },

      setValue: function(el, value) {
        const editor = el._formulaEditor;
        if (!editor) return;

        const stringValue = (value == null) ? "" : String(value);
        editor.setValue(stringValue, -1);
      },

      subscribe: function(el, callback) {
        $(el).on("formula-input:change.formulaInputBinding", function() {
          callback(false);
        });
        $(el).on("formula-input:enter.formulaInputBinding", function() {
          callback(true);
        });
      },

      unsubscribe: function(el) {
        $(el).off(".formulaInputBinding");
      },

      receiveMessage: function(el, data) {
        if (data.hasOwnProperty("value")) {
          this.setValue(el, data.value);
        }
        if (data.hasOwnProperty("terms")) {
          const highlighter = el._formulaHighlighter;
          if (highlighter) {
            highlighter.setTerms(data.terms);
          }
        }
        $(el).trigger("formula-input:change");
      },

      getRatePolicy: function() {
        return { policy: "debounce", delay: 250 };
      }
    });

    Shiny.inputBindings.register(FormulaInputBinding, "openqalytools.formulaInput");

    // Since we loaded async, Shiny may have already initialized the page.
    // Manually initialize any existing formula-input elements.
    $(".formula-input").each(function() {
      if (!this._formulaEditor) {
        FormulaInputBinding.initialize(this);
      }
    });

    // Also rebind so Shiny knows about the values
    Shiny.bindAll(document.body);

    console.log("formulaInput binding registered successfully (Ace Editor)");
  }).catch(function(error) {
    console.error("Failed to initialize formulaInput:", error);
  });
})();
