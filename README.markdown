cl-closuret-template - Common Lisp implementation of Closure Template from Google

Sample Code
-------------------------

     CL-USER> (defparameter *template* "
     /*
      *  Greets a person using 'Hello' by default.
      */
    {namespace Closure-template.Example}
    {template hello-name}
       {if not $greetingWorld}
          Hello {$name}!
       {else}
          {$greetingWorld} {$name}!
        {/if}
    {/template}")
    *TEMPLATE*
   
   
    CL-USER> (closure-template:compile-template :common-lisp-backend *template*)
    CLOSURE-TEMPLATE.EXAMPLE:HELLO-NAME
    CL-USER> (closure-template.example:hello-name '(:name "Andrey"))
    "  Hello Andrey! "
    CL-USER> (closure-template.example:hello-name '(:name "Andrey" :greeting-world "Hi"))
    "  Hi Andrey! "
    
    
    CL-USER> (closure-template:compile-template :javascript-backend *template*)
    "if (!ClosureTemplate) {
        ClosureTemplate = {  };
    };
    if (!ClosureTemplate.Example) {
        ClosureTemplate.Example = {  };
    };
    ClosureTemplate.Example.helloName = function ($$data$$) {
        var $data$ = $$data$$ || {  };
        var $templateOutput$ = '';
        $templateOutput$ += ' ';
        if (!$data$.greetingWorld) {
            $templateOutput$ += ' Hello ';
            $templateOutput$ += $data$.name;
            $templateOutput$ += '! ';
        } else {
            $templateOutput$ += ' ';
            $templateOutput$ += $data$.greetingWorld;
            $templateOutput$ += ' ';
            $templateOutput$ += $data$.name;
            $templateOutput$ += '! ';
        };
        $templateOutput$ += '';
        return $templateOutput$;
    };"
