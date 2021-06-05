"use strict";

function cnst(value) {
    return (x, y, z) => value;
}

function variable(variable_name) {
    return (x, y, z) => {
        return variable_name === 'x' ? x : variable_name === 'y' ? y : z;
    };
}

function add(first, second) {
    return (x, y, z) => first(x, y, z) + second(x, y, z);
}

function subtract(first, second) {
    return (x, y, z) => first(x, y, z) - second(x, y, z);
}

function multiply(first, second) {
    return (x, y, z) => first(x, y, z) * second(x, y, z);
}

function divide(first, second) {
    return (x, y, z) => first(x, y, z) / second(x, y, z);
}

function negate(value) {
    return (x, y, z) => -(value(x, y, z));
}

function sin(value) {
    return (x, y, z) => Math.sin(value(x, y, z))
}

function cos(value) {
    return (x, y, z) => Math.cos(value(x, y, z))
}

function avg5(...args) {
    return (x, y, z) => {
        let sum = 0;
        let count = 0;
        for (let i of args) {
            sum += i(x, y, z);
            count++;
        }
        return sum / count;
    }
}

function med3(...args) {
    return (x, y, z) => {
        let a = [];
        for (let i = 0; i < args.length; i++) {
            a.push(args[i](x, y, z));
        }
        a = a.sort((a, b) => (a - b));
        return a[Math.floor(a.length / 2)];
    }
}

let pi = cnst(Math.PI);
let e = cnst(Math.E);

function takeTokens(parseStr) {
    let tokens = [];
    let i = 0;
    let j = 0;
    while (j < parseStr.length) {
        while (j !== parseStr.length && parseStr.charAt(j) === ' ') {
            j++;
        }
        if (j === parseStr.length) {
            break;
        }
        i = j;
        while (j !== parseStr.length && parseStr.charAt(j) !== ' ') {
            j++;
        }
        tokens.push(parseStr.substr(i, j - i));
        j++;
    }
    return tokens;
}

let binary_operation = [];
binary_operation['+'] = add;
binary_operation['-'] = subtract;
binary_operation['*'] = multiply;
binary_operation['/'] = divide;

let one_argument_functions = [];
one_argument_functions['negate'] = negate;
one_argument_functions['cos'] = cos;
one_argument_functions['sin'] = sin;

let variables = new Set();
variables.add('x');
variables.add('y');
variables.add('z');

function parse(parseStr) {
    let stack = [];
    let tokens = takeTokens(parseStr);
    for (let token of tokens) {
        if (token in binary_operation) {
            let s2 = stack.pop();
            let s1 = stack.pop();
            stack.push(binary_operation[token](s1, s2));
        } else if (token in one_argument_functions) {
            let s1 = stack.pop();
            stack.push(one_argument_functions[token](s1));
        } else if (variables.has(token)) {
            stack.push(variable(token));
        } else if (token === 'e') {
            stack.push(e);
        } else if (token === 'pi') {
            stack.push(pi);
        } else if (token === 'avg5') {
            let s = [];
            for (let i = 0; i < 5; i++) {
                s.push(stack.pop());
            }
            stack.push(avg5(...s));
        } else if (token === 'med3') {
            let s = [];
            for (let i = 0; i < 3; i++) {
                s.push(stack.pop());
            }
            stack.push(med3(...s));
        } else {
            stack.push(cnst(parseInt(token)));
        }
    }
    return stack.pop();
}

function println() {
    console.log(Array.prototype.map.call(arguments, String).join(' '));
}

function test() {
    let expr = parse('x x * 2 x * - 1 +');
    let result = [];
    for (let x = 0; x <= 10; x++) {
        result.push(expr(x));
    }
    println(result);
}
