const MAX_I64 = 9223372036854775807n
const MIN_I64 = -9223372036854775808n

const MAX_TABLE_SIZE = 2048;

function safe_add(x, y) {
  const result = x + y;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + result.toString());
  }
  return result;
}

function safe_sub(x, y) {
  const result = x - y;

  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Error: Integer overflow, got " + result.toString());
  }
  return result;
}

function safe_mul(x, y) {
  const result = (x * y) / 2n;

  if (result > MAX_I64 || result < MIN_I64) {
      throw new Error("Error: Integer overflow, got " + result.toString());
  }
  return result;
}

function print(snakeval) {
  console.log(snakeval);
}

const table = new WebAssembly.Table({
  initial: MAX_TABLE_SIZE,
  element: "anyfunc"
});

const importObject = {
  runtime: { safe_add: safe_add,
             safe_sub: safe_sub,
             safe_mul: safe_mul,
             table: table
           }
};

module.exports = { importObject, print };