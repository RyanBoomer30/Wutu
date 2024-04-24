const MAX_I64 = 9223372036854775807n;
const MIN_I64 = -9223372036854775808n;

const MAX_TABLE_SIZE = 2048;
const INITIAL_MEMORY_SIZE = 1;

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

const ERR_COMP_NOT_NUM = 1;
const ERR_ARITH_NOT_NUM = 2;
const ERR_LOGIC_NOT_BOOL = 3;
const ERR_IF_NOT_BOOL = 4;
const ERR_OVERFLOW = 5;
const ERR_GET_NOT_TUPLE = 6;
const ERR_GET_LOW_INDEX = 7;
const ERR_GET_HIGH_INDEX = 8;
const ERR_GET_NOT_NUM = 9;
const ERR_NIL_DEREF = 10;
const ERR_OUT_OF_MEMORY = 11;
const ERR_SET_NOT_TUPLE = 12;
const ERR_SET_LOW_INDEX = 13;
const ERR_SET_NOT_NUM = 14;
const ERR_SET_HIGH_INDEX = 15;
const ERR_CALL_NOT_CLOSURE = 16;
const ERR_CALL_ARITY_ERR = 17;
const ERR_TUPLE_SIZE_MISMATCH = 18;
const ERR_PATTERN_NOT_TUPLE = 19;

// helper for the safe_op functions, which perform tag checking
function tag_check_arith(x, y) {
  if (x % 2n !== 0n) {
    // when we throw an error, the cause is the error code to exit with (for consistency)
    throw new Error(
      "Error: arithmetic expected a number, got " + snake_to_string(x),
      { cause: 2 }
    );
  } else if (y % 2n !== -0n) {
    throw new Error(
      "Error: arithmetic expected a number, got " + snake_to_string(y),
      { cause: 2 }
    );
  }
}

// safe functions in JS to detect overflow, since Wasm doesn't let us do it directly
// while we're here, we might as well just do tag checks too!
function safe_add(x, y) {
  tag_check_arith(x, y);
  const result = x + y;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + snake_to_string(result), {
      cause: 5,
    });
  }
  return result;
}

function safe_sub(x, y) {
  tag_check_arith(x, y);
  const result = x - y;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + snake_to_string(result), {
      cause: 5,
    });
  }
  return result;
}

function safe_mul(x, y) {
  tag_check_arith(x, y);
  const result = (x * y) / 2n;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + snake_to_string(result), {
      cause: 5,
    });
  }
  return result;
}

function error(snakeval, code) {
  // we pass the code in Wasm as an i64, but it is easier to work with as i32
  code = Number(code);

  switch (code) {
    case ERR_COMP_NOT_NUM:
      throw new Error(
        "Error: comparison expected a number, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_ARITH_NOT_NUM:
      throw new Error(
        "Error: arithmetic expected a number, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_LOGIC_NOT_BOOL:
      throw new Error(
        "Error: logic expected a boolean, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_IF_NOT_BOOL:
      throw new Error(
        "Error: if expected a boolean, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_OVERFLOW:
      throw new Error(
        "Error: Integer overflow, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_GET_NOT_TUPLE:
      throw new Error(
        "Error: get expected tuple, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_GET_LOW_INDEX:
      throw new Error(
        "Error: index too small to get, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_GET_HIGH_INDEX:
      throw new Error(
        "Error: index too large to get, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_GET_NOT_NUM:
      throw new Error(
        "Error: get expected numeric index, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_NIL_DEREF:
      throw new Error("Error: tried to access component of nil", {
        cause: code,
      });
    case ERR_OUT_OF_MEMORY:
      throw new Error("Error: out of memory", { cause: code });
    case ERR_SET_NOT_TUPLE:
      throw new Error(
        "Error: set expected tuple, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_SET_LOW_INDEX:
      throw new Error(
        "Error: index too small to set, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_SET_NOT_NUM:
      throw new Error(
        "Error: set expected numeric index, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_SET_HIGH_INDEX:
      throw new Error(
        "Error: index too large to set, got " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_CALL_NOT_CLOSURE:
      throw new Error(
        "Error: tried to call a non-closure value, got " +
          snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_CALL_ARITY_ERR:
      throw new Error("Error: arity mismatch in call", { cause: code });
    case ERR_TUPLE_SIZE_MISMATCH:
      throw new Error(
        "Error: pattern expected a tuple of size " + snake_to_string(snakeval),
        { cause: code }
      );
    case ERR_PATTERN_NOT_TUPLE:
      throw new Error(
        "Error: pattern expected a tuple, got " + snake_to_string(snakeval),
        { cause: code }
      );
    default:
      throw new Error(
        "Error: Unknown error code: " +
          code +
          ", val: " +
          snake_to_string(snakeval),
        { cause: code }
      );
  }
}

function snake_to_string(snakeval) {
  unsigned = BigInt.asUintN(64, snakeval); // interpret as unsigned

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

const memory = new WebAssembly.Memory({
  initial: INITIAL_MEMORY_SIZE,
});

const importObject = {
  runtime: {
    safe_add: safe_add,
    safe_sub: safe_sub,
    safe_mul: safe_mul,
    table: table,
    memory: memory,
    error: error,
  },
};

function print_table(tagged_ptr) {
  const mem = new BigInt64Array(memory.buffer);

  // we untag, then shift because we access with words but ptr is in bytes
  for (i = 0; i < 10; i++) {
    console.log(mem[i]);
  }
}

module.exports = { importObject, print, print_table };
