const MAX_I64 = 9223372036854775807n;
const MIN_I64 = -9223372036854775808n;

const MAX_TABLE_SIZE = 2048;

const NUM_TAG_MASK = 0x0000000000000001n;
const BOOL_TAG_MASK = 0x000000000000000fn;
const TUPLE_TAG_MASK = 0x0000000000000007n;
const CLOSURE_TAG_MASK = 0x0000000000000007n;
const FWDPTR_TAG_MASK = 0x0000000000000007n;
const NUM_TAG = 0x0000000000000000n;
const BOOL_TAG = 0x000000000000000fn;
const TUPLE_TAG = 0x0000000000000001n;
const CLOSURE_TAG = 0x0000000000000005n;
const FWDPTR_TAG = 0x0000000000000003n;
const BOOL_TRUE = 0xffffffffffffffffn;
const BOOL_FALSE = 0x7fffffffffffffffn;
const NIL = 0n | TUPLE_TAG;

// helper for the safe_op functions, which perform tag checking
function tag_check_arith(x, y) {
  if (x % 2n !== 0n) {
    // when we throw an error, the cause is the error code to exit with (for consistency)
    throw new Error("Error: arithmetic expected a number, got " + snake_to_string(x), { cause: 2 });
  } else if (y % 2n !== -0n) {
    throw new Error("Error: arithmetic expected a number, got " + snake_to_string(y), { cause: 2 });
  }
}

// safe functions in JS to detect overflow, since Wasm doesn't let us do it directly
// while we're here, we might as well just do tag checks too!
function safe_add(x, y) {
  tag_check_arith(x, y);
  const result = x + y;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + snake_to_string(result), { cause: 5 });
  }
  return result;
}

function safe_sub(x, y) {
  tag_check_arith(x, y);
  const result = x - y;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + snake_to_string(result), { cause: 5 });
  }
  return result;
}

function safe_mul(x, y) {
  tag_check_arith(x, y);
  const result = (x * y) / 2n;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + snake_to_string(result), { cause: 5 });
  }
  return result;
}

function error(code, snakeval) {
  throw new Error("We need to implement this one still");
}

function snake_to_string(snakeval) {
  unsigned = BigInt.asUintN(64, snakeval);  // interpret as unsigned

  if (unsigned === BOOL_TRUE) {
    return "true";
  } else if (unsigned === BOOL_FALSE) {
    return "false";
  } else if (unsigned === NIL) {
    return "nil";
  } else if ((unsigned & NUM_TAG_MASK) === NUM_TAG) {
    return (snakeval >> 1n).toString();
  } else {
    return "Unknown value: " + unsigned.toString();
  }
}

function print(snakeval) {
  console.log(snake_to_string(snakeval));
  // figure out how to get rid of trailing newlines above
  // maybe we have to return strings and concat them together
  // only hard part is to print out the tuples correctly
  // OR, maybe we could convert to JS arrays and print those?
  // that will print with [ ], which isn't what we want, so we may
  // just have to port over out C code
  return snakeval;
}

const table = new WebAssembly.Table({
  initial: MAX_TABLE_SIZE,
  element: "anyfunc",
});

const importObject = {
  runtime: {
    safe_add: safe_add,
    safe_sub: safe_sub,
    safe_mul: safe_mul,
    table: table,
    error: error
  },
};

module.exports = { importObject, print };