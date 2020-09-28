/// Basic Types

// boolean
let isDone: boolean = false;
// or
const truth: boolean = true;

// number literals
let decimal: number = 6;
let hex: number = 0xf00d;
let binary: number = 0b1010;
let octal: number = 0o744;

// string
let color: string = 'blue';
// interpolation
let sentence: string = `My favorite color is ${color}`;

// list
let list: Array<number> = [1, 2, 3];

// tuples (seems dumb)
let x: [string, number];
// Initialize it
x = ["hello", 10]; // OK
// Initialize it incorrectly
x = [10, "hello"]; // Error

// enum
enum Color {Red, Green, Blue}
let c: Color = Color.Green;

function warnUser(): void {
  console.log("This is my warning message");
}

// undefined and null are also types
let u: undefined = undefined;
let n: null = null;

// object is a type that represents the non-primitive type
// i.e. anything that is not number, string, boolean, bigint, symbol, null, or undefined.
let o: object = {foo: "bar"};

// there's also an escape hatch
let someValue: any = "don't do this";

// type assertions/casts
let strLength: number = (<string>someValue).length;
// same as
let strLength: number = (someValue as string).length;

/// Variable Declaration

// always prefer const
const x: number = 5;

// array destructuring
let arr: Array<string> = ["foo", "bar"];
let [f, b] = arr;

// object destructuring
let obj: object = {a: 300, b: "sparta"}
let {a, b} = obj;
// with renaming
let {a: count, b: polis} = obj;
console.log(`We ${count} of ${polis}`)

// spread
let first = [1, 2];
let second = [3, 4]; 
let bothPlus = [0, ...first, ...second, 5];

let defaults = { food: "spicy", price: "$$", ambiance: "noisy" };
let search = { ...defaults, food: "rich" };

/// Interfaces
