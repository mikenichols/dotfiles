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
let myTuple: [string, number];
// Initialize it
myTuple = ["hello", 10]; // OK
// Initialize it incorrectly
myTuple = [10, "hello"]; // Error

// enum
enum Color { Red, Green, Blue }
let c: Color = Color.Green;
// Really just ints starting at 0
enum Direction { Up = 3, Down = 5, Left = 8, Right = 13 }
console.log(Direction[5]) // Prints the string 'Down'

// void is the abscence of a type
function warnUser(): void {
  console.log("This is my warning message");
}
// never is bottom
function boom(message: string): never {
  throw new Error(message);
}

// undefined and null are also types
let u: undefined = undefined;
let n: null = null;

// object is a type that represents the non-primitive type
// i.e. anything that is not number, string, boolean, bigint, symbol, null, or undefined.
let o: object = { foo: "bar" };

// there's also an escape hatch
let someValue: any = "don't do this";

// type assertions/casts
let strLength1: number = (<string>someValue).length;
// same as
let strLength2: number = (someValue as string).length;

// Note that String and string are not equivalent, same with
// Number, Boolean, Symbol, and Object

/// Variable Declaration

// always prefer const
const x: number = 5;

// array destructuring
let arr1: Array<string> = ["foo", "bar"];
let [f, g] = arr1;

// object destructuring
let obj: object = { a: 300, b: "sparta" }
let { a, b } = obj;
// with renaming
let { c: count, d: polis } = obj;
console.log(`We ${count} of ${polis}`)

// spread
let first = [1, 2];
let second = [3, 4];
let bothPlus = [0, ...first, ...second, 5];

let defaults = { food: "spicy", price: "$$", ambiance: "noisy" };
let search = { ...defaults, food: "rich" };

// Check this out!
let arr2: ReadonlyArray<number> = [1, 2, 3];

/// Interfaces

// There are two ways to declare types: interface and type
// Prefer interface unless you need the features of type

// An interface is just the shape of data
interface MyInterface {
  label: string;
  optional?: number;
  readonly awesome: boolean;
}
function printLabel(obj: MyInterface) {
  console.log(obj.label);
}
let myObj = { size: 10, label: "Size 10 Object", awesome: true }
printLabel(myObj);

/// Functions

interface MultipleOf {
  (a: number, b: number): boolean;
}

let multipleOf: MultipleOf;

// Note that the param names don't have to match
multipleOf = function(x: number, y: number): boolean {
  return x % y === 0;
};


/// Types

// Unions
type WindowStates = "open" | "closed" | "minimized";
