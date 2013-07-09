cl-closure-template - Common Lisp implementation of Closure Template from Google

Sample Code
-------------------------
     CL-USER> (asdf:operate 'asdf:load-op '#:closure-template)

     CL-USER> (defparameter *template* "
     /*
      *  Greets a person using 'Hello' by default.
      */
    {namespace closureTemplate.Example}
    {template helloName}
       {if not $greetingWorld}
          Hello {$name}!
       {else}
          {$greetingWorld} {$name}!
        {/if}
    {/template}")
    *TEMPLATE*
   
   
    CL-USER> (closure-template:compile-template :common-lisp-backend *template*)
    #<PACKAGE "CLOSURETEMPLATE.EXAMPLE">
    CL-USER> (closuretemplate.example:hello-name '(:name "Andrey"))
    "Hello Andrey!"
    CL-USER> (closure-template.example:hello-name '(:name "Andrey" :greeting-world "Hi"))
    "Hi Andrey!"
    
    
    CL-USER> (closure-template:compile-template :javascript-backend *template*)
    "if (typeof closureTemplate === 'undefined') { closureTemplate = {}; }
    if (typeof closureTemplate.Example === 'undefined') { closureTemplate.Example = {}; }
    
    closureTemplate.Example.$isEmpty$ = function (obj) {
        for (var prop in obj) if (obj.hasOwnProperty(prop)) return false;
        return true;
    };
    
    closureTemplate.Example.$escapeHTML$ = function (obj) {
        if (typeof obj == 'string') return String(obj).split('&').join('&amp;').split( '<').join('&lt;').split('>').join('&gt;').split('\\\"').join('&quot;').split('\\'').join('&#039;');
        else return obj;
    };
    
    closureTemplate.Example.$round$ = function (number, ndigits) {
        if (ndigits) {
            var factor = Math.pow(10.0, ndigits);
            return Math.round(number * factor) / factor;
        }
        else return Math.round(number)
    };
    
    closureTemplate.Example.$objectFromPrototype$ = function (obj) {
        function C () {}
        C.prototype = obj;
        return new C;
    };
    
    closureTemplate.Example.helloName = function($env$, $target$) {
        if (!$env$) { $env$ = {}; }
        var $result$ = $target$ || [];
    
        if (!$env$.greetingWorld) {
            $result$.push(\"Hello \");
            $result$.push(closureTemplate.Example.$escapeHTML$($env$.name));
            $result$.push(\"!\");
        }
        else {
            $result$.push(closureTemplate.Example.$escapeHTML$($env$.greetingWorld));
            $result$.push(\" \");
            $result$.push(closureTemplate.Example.$escapeHTML$($env$.name));
            $result$.push(\"!\");
        }
    
        if (!$target$) return $result$.join('');
        else return null;
    };
    "

    CL-USER> (closure-template:compile-template :requirejs-backend *template*)
    "define(function () {
    var module = { };
    module.$isEmpty$ = function (obj) {
        for (var prop in obj) if (obj.hasOwnProperty(prop)) return false;
        return true;
    };
    
    module.$escapeHTML$ = function (obj) {
        if (typeof obj == 'string') return String(obj).split('&').join('&amp;').split( '<').join('&lt;').split('>').join('&gt;').split('\\\"').join('&quot;').split('\\'').join('&#039;');
        else return obj;
    };
    
    module.$round$ = function (number, ndigits) {
        if (ndigits) {
            var factor = Math.pow(10.0, ndigits);
            return Math.round(number * factor) / factor;
        }
        else return Math.round(number)
    };
    
    module.$objectFromPrototype$ = function (obj) {
        function C () {}
        C.prototype = obj;
        return new C;
    };
    
    module.helloName = function($env$, $target$) {
        if (!$env$) { $env$ = {}; }
        var $result$ = $target$ || [];
    
        if (!$env$.greetingWorld) {
            $result$.push(\"Hello \");
            $result$.push(module.$escapeHTML$($env$.name));
            $result$.push(\"!\");
        }
        else {
            $result$.push(module.$escapeHTML$($env$.greetingWorld));
            $result$.push(\" \");
            $result$.push(module.$escapeHTML$($env$.name));
            $result$.push(\"!\");
        }
    
        if (!$target$) return $result$.join('');
        else return null;
    };
    return module; });"

Adding Custom Print Directives
------------------------------

You can add custom print directives. For example, printing integers as
hexadecimal values:

    CL-USER> (closure-template:define-print-syntax printHex "hex" (:constant t)) 
    CLOSURE-TEMPLATE.PARSER::PRINT-DIRECTIVE
    CL-USER> (closure-template:register-print-handler :common-lisp-backend 'printHex :function #'(lambda (params end value) (format nil "~X" value)))
    #<Anonymous Function #x302001B7085F>
    CL-USER> (defparameter *template* "
         /*
          *  Greets a person using 'Hello' by default.
          */
        {namespace closureTemplate.Example}
        {template helloName}
        Hello {$name} {$param|hex}!
        {/template}")
    *TEMPLATE*
    CL-USER> (closure-template:compile-template :common-lisp-backend *template*)
    #<Package "CLOSURE-TEMPLATE.EXAMPLE">
    CL-USER> (closure-template.example:hello-Name '(:name "Name" :param 128))
    "Hello Name 80!"

To use Javascript backend, you need to register a handler for Javascript or
RequireJS backends. This is a more complex example which shows how to handle
parameters of the directive:

    CL-USER> (closure-template:define-print-syntax printHex (and "hex" (esrap:? (and ":" (or "upper" "lower"))))
               (:destructure (tag value)
                             (declare (ignore tag))
                             (list (if (and value (string= (second value) "lower")) 'case-lower 'case-upper))))
    PRINT-HEX
    CL-USER> (closure-template:register-print-handler :common-lisp-backend 'printHex
                                                      :function #'(lambda (params env value)
                                                                    (declare (ignore env))
                                                                    (format nil (if (member 'case-lower params) "~(~X~)" "~X") value)))
    PRINT-HEX
    CL-USER> (closure-template:register-print-handler :javascript-backend 'printHex
                                             :handler "function (params, value) { var result = value.toString(16); if (params.case == \"upper\") { result = result.toUpperCase(); } return result; }"
                                             :parameter-converter #'(lambda (params)
                                                                      (format nil "{ case: \"~A\"; }" (if (member 'case-lower params) "lower" "upper"))))
    
    PRINTHEX
    CL-USER> (defparameter *template* "
         /*
          *  Greets a person using 'Hello' by default.
          */
        {namespace closureTemplate.Example}
        {template helloName}
        Hello {$name} {$param|hex:lower}!
        {/template}")
    *TEMPLATE*
    CL-USER> (closure-template.example:hello-name '(name "Name" :param 143))
    "Hello Name 8F!"

You can also use a predefined function by name or RequireJS module. 
For detals, please see docstrings for REGISTER-PRINT-HANDLER implementations.

