//

var ClosureTemplate = {  };

ClosureTemplate.helloWorld = function (data, out) {
    out.push("Hello world");
};

ClosureTemplate.Test = {  };

// Simple

ClosureTemplate.Test.testSimple = function () {
    this.assertEqual('Hello world!', closureTemplate.js.testSimple1());
    this.assertEqual('<Hello world>', closureTemplate.js.testSimple2());
};

// Print

ClosureTemplate.Test.testPrint = function () {
    this.assertEqual('&lt;&amp;&quot;&#039;&gt;',
                     closureTemplate.js.testPrint1({arg: '<&\"\'>'}));  
    this.assertEqual('<&\"\'>',
                     closureTemplate.js.testPrint2({arg: '<&\"\'>'}));  
    this.assertEqual('<&\"\'>',
                     closureTemplate.js.testPrint3({arg: '<&\"\'>'}));  
    this.assertEqual('~!@#$%25%5E&*()%7B%7D%5B%5D=:/,;?+\'%22%5C',
                     closureTemplate.js.testPrint4({arg: '~!@#$%^&*(){}[]=:/,;?+\'\"\\'}));  
    this.assertEqual('~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B\'%22%5C',
                     closureTemplate.js.testPrint5({arg: '~!@#$%^&*(){}[]=:/,;?+\'\"\\'}));  
    this.assertEqual('&lt;&amp;&quot;&#039;&gt;',
                     closureTemplate.js.testPrint6({arg: '<&\"\'>'}));
};

// Comment

ClosureTemplate.Test.testComment = function () {
    this.assertEqual('Hello world',
                     closureTemplate.js.testComment1());
    this.assertEqual('Hello world',
                     closureTemplate.js.testComment2());
};

// Calculate

ClosureTemplate.Test.testCalculate = function () {
    this.assertEqual('20',
                     closureTemplate.js.testCalculate1());
    this.assertEqual('20',
                     closureTemplate.js.testCalculate2());
    this.assertEqual('2',
                     closureTemplate.js.testCalculate3());

    this.assertEqual('Hello world',
                     closureTemplate.js.testCalculate4());
    this.assertEqual('10',
                     closureTemplate.js.testCalculate4({a: 1}));


    this.assertEqual('5',
                     closureTemplate.js.testCalculate6({ x: 2, y: 3 }));
    this.assertEqual('Hello world',
                     closureTemplate.js.testCalculate6({ x: 'Hello ', y: 'world' }));
    this.assertEqual('Number: 6',
                     closureTemplate.js.testCalculate6({ x: 'Number: ', y: 6 }));


    this.assertEqual('3',
                     closureTemplate.js.testCalculate7());
    this.assertEqual('3',
                     closureTemplate.js.testCalculate7({}));
    this.assertEqual('2.72',
                     closureTemplate.js.testCalculate7({ num: 2 }));
    this.assertEqual('2.7183',
                     closureTemplate.js.testCalculate7({ num: 4 }));

    this.assertEqual('1',
                     closureTemplate.js.testCalculate8({ array: [0, 1, 4, 9, 16, 25, 36],
                                                         index: 1 }));
    this.assertEqual('16',
                     closureTemplate.js.testCalculate8({ array: [0, 1, 4, 9, 16, 25, 36],
                                                         index: 4 }));
    this.assertEqual('9',
                     closureTemplate.js.testCalculate8({ array: [0, 1, 4, 9, 16, 25, 36],
                                                         index: 3 }));
    this.assertEqual('36',
                     closureTemplate.js.testCalculate8({ array: [0, 1, 4, 9, 16, 25, 36],
                                                         index: 6 }));

    this.assertEqual('false',
                     closureTemplate.js.testCalculate9({ val: 6 }));
    this.assertEqual('true',
                     closureTemplate.js.testCalculate9({ val: 5 }));

    this.assertEqual('true',
                     closureTemplate.js.testCalculate10({ val: 6 }));
    this.assertEqual('false',
                     closureTemplate.js.testCalculate10({ val: 5 }));

    this.assertEqual('3',
                     closureTemplate.js.testCalculate11({ array: [0, 1, 2] }));
    this.assertEqual('7',
                     closureTemplate.js.testCalculate11({ array: [0, 1, 2, 3, 4, 5, 6] }));

    this.assertEqual('True',  closureTemplate.js.testCalculate12({ str1: 'Blue Whale', str2: 'Blue'}));
    this.assertEqual('False', closureTemplate.js.testCalculate12({ str1: 'Blue Whale', str2: 'Blute'}));
    this.assertEqual('True',  closureTemplate.js.testCalculate12({ str1: 'Blue Whale', str2: 'Wha'}));
    this.assertEqual('False', closureTemplate.js.testCalculate12({ str1: 'Blue Whale', str2: 'What'}));
    this.assertEqual('True',  closureTemplate.js.testCalculate12({ str1: 'Blue Whale', str2: ''}));

    this.assertEqual('True',  closureTemplate.js.testCalculate13({x: 1}));
    this.assertEqual('False', closureTemplate.js.testCalculate13({y: 1}));
    
};

// Random

ClosureTemplate.Test.testRandom = function () {
    for (var i = 0; i < 100; ++i) {
        var rnd = parseInt(closureTemplate.js.testCalculate5());

        
        this.assert(rnd >= 0);
        this.assert(rnd < 10);
    }
};


// Substitions

ClosureTemplate.Test.testSubstition = function () {
    this.assertEqual(' \r\n\t{}',
                     closureTemplate.js.testSubstitions());
};

// Injected data

ClosureTemplate.Test.testInjectedData = function () {
    this.assertEqual('Hello world', closureTemplate.js.testInjectedData1({ foo: 'Hello' }, null, {foo: 'world'} ));
    this.assertEqual('Hello world', closureTemplate.js.testInjectedData2({ foo: 'Hello' }, null, {foo: 'world'}));
};

// Dotted

ClosureTemplate.Test.testDottedVariables = function () {
    this.assertEqual('Hello world',
                     closureTemplate.js.testDotted1({ obj: { first: 'Hello', second: 'world' } }));
    this.assertEqual('Hello world',
                     closureTemplate.js.testDotted2({ obj: { msg: { first: 'Hello', second: 'world' } } }));
};

// Local variables

ClosureTemplate.Test.testLocalVariables = function () {
    this.assertEqual('56',
                     closureTemplate.js.testLocal1({ c: [{ d: 5 }, { d: 6}]}));
    this.assertEqual('56',
                     closureTemplate.js.testLocal2({ c: [{ d: { a: 5 } }, { d: { a: 6 } }] }));

};

// Literals

ClosureTemplate.Test.testLiteral = function () {
    this.assertEqual('\'"&{$x}{}"',
                     closureTemplate.js.testLiteral1());
    this.assertEqual('Hello world!\nBy world!',
                     closureTemplate.js.testLiteral2());
};

// With

ClosureTemplate.Test.testWith = function () {
    this.assertEqual('Hello Andrey',
                     closureTemplate.js.testWith1());
    this.assertEqual('Hello Andrey',
                     closureTemplate.js.testLet1());

};

// If

ClosureTemplate.Test.testIf = function () {
    this.assertEqual('Hello Andrey',
                     closureTemplate.js.testIf1({ name: 'Andrey' }));
    this.assertEqual('',
                     closureTemplate.js.testIf1());

    this.assertEqual('Hello Andrey',
                     closureTemplate.js.testIf2({ name: 'Andrey' }));
    this.assertEqual('Hello Guest',
                     closureTemplate.js.testIf2({}));


    this.assertEqual('Hello Andrey',
                     closureTemplate.js.testIf3({ hello: 'Andrey' }));
    this.assertEqual('By Masha',
                     closureTemplate.js.testIf3({ by: 'Masha' }));
    this.assertEqual('Thank Vasy',
                     closureTemplate.js.testIf3( { thank: 'Vasy' }));
    this.assertEqual('Guest?',
                     closureTemplate.js.testIf3({}));
};

// Switch

ClosureTemplate.Test.testSwitch = function () {
    this.assertEqual('Variant 1: 0',
                    closureTemplate.js.testSwitch1({ 'var': 0 }));
    this.assertEqual('Variant 2: Hello',
                    closureTemplate.js.testSwitch1({ 'var': 'Hello' }));
    this.assertEqual('Miss!',
                    closureTemplate.js.testSwitch1());
    this.assertEqual('Variant 2: 2',
                    closureTemplate.js.testSwitch1({ 'var': 2 }));

    this.assertEqual('Variant 1: 0',
                    closureTemplate.js.testSwitch2({ 'var': 0 }));
    this.assertEqual('Variant 2: Hello',
                    closureTemplate.js.testSwitch2({ 'var': 'Hello' }));
    this.assertEqual('',
                    closureTemplate.js.testSwitch2({}));
    this.assertEqual('Variant 2: 2',
                    closureTemplate.js.testSwitch2({ 'var': 2 }));

};

// Foreach

ClosureTemplate.Test.testForeach = function () {
    this.assertEqual(' alpha beta gamma',
                     closureTemplate.js.testForeach1({ opernands: ["alpha", "beta", "gamma"] }));
    this.assertEqual(' alpha beta gamma',
                     closureTemplate.js.testForeach2({ opernands: ["alpha", "beta", "gamma"] }));
    this.assertEqual('Hello world',
                     closureTemplate.js.testForeach2({}));
    this.assertEqual('012',
                     closureTemplate.js.testForeach3({ opernands: [1, 2, 3] }));
    this.assertEqual('alpha+beta+gamma',
                     closureTemplate.js.testForeach4({ opernands: ["alpha", "beta", "gamma"] }));
    this.assertEqual('alpha+beta+gamma',
                     closureTemplate.js.testForeach5({ opernands: ["alpha", "beta", "gamma"] }));
};

// List

ClosureTemplate.Test.testList = function () {
    this.assertEqual('123',
                     closureTemplate.js.testList1());
    this.assertEqual('AB',
                     closureTemplate.js.testList2({a: 'A', b: 'B'}));
};

// Map

ClosureTemplate.Test.testMap = function () {
    this.assertEqual('123', closureTemplate.js.testMap1());
    this.assertEqual('Hello Masha', closureTemplate.js.testMap2());
    this.assertEqual('abc', closureTemplate.js.testMap3());
    this.assertEqual('423', closureTemplate.js.testMap4());
    this.assertEqual('abcd', closureTemplate.js.testMap5().split("").sort().join(""));
};

// For

ClosureTemplate.Test.testFor = function () {
    this.assertEqual('01234',
                    closureTemplate.js.testFor1());
    this.assertEqual('456789',
                    closureTemplate.js.testFor2());
    this.assertEqual('147',
                    closureTemplate.js.testFor3({ from: 1, to: 10, by: 3 }));
    this.assertEqual('5152 6162 7172 ',
                     closureTemplate.js.testFor4({}));
};

// Call

ClosureTemplate.Test.testCall = function () {
    this.assertEqual('Hello world',
                    closureTemplate.js.testCall1());
    this.assertEqual('Hello Andrey',
                    closureTemplate.js.testCall2());
    this.assertEqual('Hello Andrey',
                    closureTemplate.js.testCall3());
    this.assertEqual('Hello Masha',
                    closureTemplate.js.testCall4({ name: 'Masha' }));
    this.assertEqual('Hello Hello Ivan',
                    closureTemplate.js.testCall5({'name': 'Ivan'}));
    this.assertEqual('Hello Masha',
                    closureTemplate.js.testCall6({author: {'name': 'Masha'}}));
    this.assertEqual('Hello Andrey from Krasnodar',
                    closureTemplate.js.testCall7({'author': {name: 'Masha', city: 'Krasnodar'}}));
    this.assertEqual('Hello world',
                    closureTemplate.js.testCall8());
    this.assertEqual('Hello Andrey',
                    closureTemplate.js.testCall9());
    this.assertEqual('Hello world',
                    closureTemplate.js.testCall10());
};