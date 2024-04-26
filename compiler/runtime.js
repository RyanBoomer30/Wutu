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

const WORD_SIZE = 8;

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

function snake_to_string(snakeval, mem) {
  let unsigned = BigInt.asUintN(64, snakeval); // interpret as unsigned

  if (unsigned === BOOL_TRUE) {
    return "true";
  } else if (unsigned === BOOL_FALSE) {
    return "false";
  } else if (unsigned === NIL) {
    return "nil";
  } else if ((unsigned & CLOSURE_TAG_MASK) === CLOSURE_TAG) {
    return "<function>";
  } else if ((unsigned & NUM_TAG_MASK) === NUM_TAG) {
    return (snakeval >> 1n).toString();
  } else if ((unsigned & TUPLE_TAG_MASK) === TUPLE_TAG) {
    let untagged = Number(snakeval - TUPLE_TAG);

    let addr = untagged / WORD_SIZE;

    let size = mem[addr] >> 1n; // convert from SNAKEVAL to actual value (in words)

    let res = "(";
    for (let i = 1; i <= size; i++) {
      if (i != 1) {
        res += ", ";
      }
      res += snake_to_string(mem[addr + i], mem);
    }
    // for compatibility with Racer
    if (size == 1) {
      res += ", ";
    }
    return res + ")";
  } else {
    return "Unknown value: " + unsigned.toString();
  }
}

function print(snakeval) {
  let mem = new BigInt64Array(memory.buffer);
  console.log(snake_to_string(snakeval, mem));
  return snakeval;
}

function equal(v1, v2) {
  let mem = new BigInt64Array(memory.buffer);

  if (equal_help(v1, v2, mem)) {
    return BOOL_TRUE;
  }
  return BOOL_FALSE;
}

function equal_help(v1, v2, mem) {
  let unsigned_v1 = BigInt.asUintN(64, v1);
  let unsigned_v2 = BigInt.asUintN(64, v2);

  // if both are tuples, we have to recur
  if (
    (unsigned_v1 & TUPLE_TAG_MASK) === TUPLE_TAG &&
    (unsigned_v2 & TUPLE_TAG_MASK) === TUPLE_TAG
  ) {
    // if both are nil, then they are equal
    if (unsigned_v1 === NIL && unsigned_v2 === NIL) {
      return true;
    }
    if (unsigned_v1 === NIL || unsigned_v2 === NIL) {
      return false;
    }

    let untagged_v1 = Number(v1 - TUPLE_TAG);
    let untagged_v2 = Number(v2 - TUPLE_TAG);

    // we have to divide by word size to convert from bytes to words,
    let addr_v1 = untagged_v1 / WORD_SIZE;
    let addr_v2 = untagged_v2 / WORD_SIZE;

    // then shift to go from SNAKEVAL to normal value
    let size_v1 = mem[addr_v1] >> 1n;
    let size_v2 = mem[addr_v2] >> 1n;

    if (size_v1 != size_v2) {
      return false;
    }

    for (let i = 1; i <= size_v1; i++) {
      if (!equal_help(mem[addr_v1 + i], mem[addr_v2 + i], mem)) {
        return false;
      }
    }
    // if they make it past the for loop above, they are the same
    return true;
  }

  // otherwise, fall back to normal equality
  return v1 === v2;
}

// Only from browser
function input() {  
  let res = prompt("Enter in a number or literal").toLowerCase();

  if (res === "true") {
    return BOOL_TRUE;
  }
  if (res === "false") {
    return BOOL_FALSE;
  }
  if (res === "nil") {
    return NIL;
  }

  try {
    return BigInt(res);
  } catch (e) {
    alert("That was not a valid entry, try again!");
    return input();
  }
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
    print: print,
    equal: equal,
    input: input,
  },
};

function print_table() {
  const mem = new BigInt64Array(memory.buffer);

  // we untag, then shift because we access with words but ptr is in bytes
  for (i = 0; i < 16; i++) {
    console.log(mem[14 + i]);
  }
}

// module.exports = { importObject, print, print_table };
export { importObject, print };