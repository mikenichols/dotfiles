// JavaScript Cheatsheet
const assert = require('assert');

// Comments
// Hello world
console.log('Hello World!');
// Print interpolation
city = "Chicago"
console.log(`Hello ${city}!`)

// Built-ins
// true/false
facts = true;
some_value = false;
// null value
billion_dollar_mistake = null
also_this_one = undefined;
// if/else-if/else, and/or
if (facts && facts) {
  // one thing
} else if (!some_value || true) {
  // another
} else {
  // something else
}
// switch
switch (some_value) {
case true:
  console.log('some_value is true');
  break;
case false:
  console.log('some_value is NOT true');
  break;
default:
  console.log('ERROR')
}
// exceptions
if (false) {
  throw new Error('boom!');
}
// function syntax
function do_something(arg1, arg2) {
  return 123;
}
const do_something_else = (arg1, arg2) => {
  return 456;
}
assert(do_something() == 123);
assert(do_something_else() == 456);

// Math
// exponentiation
assert(2 ** 3 == 8)

// Strings
let word = 'foobar';
// split
let letters = word.split('');
// sort (destructive)
letters.sort();
// join
new_word = letters.join('');
// substring

// Regex syntax

// Arrays/Lists
// literal
// iteration
// sort
let sorted_array = [3,1,4,2].sort()
assert(sorted_array[0] == 1);
assert(sorted_array[1] == 2);
assert(sorted_array[2] == 3);
assert(sorted_array[3] == 4);
// custom sorting
// splicing
// merge sort
// quick sort

// Hash
// default value
// key existence
// value existence
// group by
// combining 2 hashes

// Class syntax
class MyClass {
  constructor(arg1, arg2) {
    this.arg1 = arg1;
    this.arg2 = arg2;
  }

  some_func(foo) {
    if (foo < arg1) {
      return foo;
    } else {
      return arg1;
    }
  }
}
// methods

// Node
// Linked list example
// Tree example
// Graph example
