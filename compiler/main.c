#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "gc.h"

typedef uint64_t SNAKEVAL;

extern SNAKEVAL our_code_starts_here(uint64_t *HEAP, int size) asm("our_code_starts_here");
extern void error(uint64_t err_code, SNAKEVAL val) asm("error");
extern SNAKEVAL print(SNAKEVAL val) asm("print");
extern SNAKEVAL input() asm("input");
extern SNAKEVAL printStack(SNAKEVAL val, uint64_t* rsp, uint64_t* rbp, int args) asm("print_stack");
extern SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) asm("equal");
extern SNAKEVAL add_trigger(SNAKEVAL val) asm("add_trigger");
extern SNAKEVAL alert_val(SNAKEVAL val) asm("alert_val");
extern SNAKEVAL display_val(SNAKEVAL val) asm("display_val");
extern SNAKEVAL big_bang(SNAKEVAL on_tick, SNAKEVAL render, SNAKEVAL value, SNAKEVAL should_world_end) asm("big_bang");
extern uint64_t* try_gc(uint64_t* alloc_ptr, uint64_t amount_needed, uint64_t* first_frame, uint64_t* stack_top) asm("?try_gc");
extern uint64_t* STACK_BOTTOM asm("STACK_BOTTOM");
extern uint64_t* HEAP_END asm("?HEAP_END");
extern uint64_t* HEAP asm("?HEAP");

size_t HEAP_SIZE;
uint64_t* STACK_BOTTOM;
uint64_t* HEAP;
uint64_t* HEAP_END;

uint64_t* FROM_S;
uint64_t* FROM_E;
uint64_t* TO_S;
uint64_t* TO_E;

const uint64_t NUM_TAG_MASK     = 0x0000000000000001;
const uint64_t BOOL_TAG_MASK    = 0x000000000000000F;
const uint64_t TUPLE_TAG_MASK   = 0x0000000000000007;
const uint64_t CLOSURE_TAG_MASK = 0x0000000000000007;
const uint64_t FWDPTR_TAG_MASK  = 0x0000000000000007;
const uint64_t NUM_TAG          = 0x0000000000000000;
const uint64_t BOOL_TAG         = 0x000000000000000F;
const uint64_t TUPLE_TAG        = 0x0000000000000001;
const uint64_t CLOSURE_TAG      = 0x0000000000000005;
const uint64_t FWDPTR_TAG       = 0x0000000000000003;
const uint64_t BOOL_TRUE        = 0xFFFFFFFFFFFFFFFF;
const uint64_t BOOL_FALSE       = 0x7FFFFFFFFFFFFFFF;
const uint64_t NIL              = ((uint64_t)NULL | TUPLE_TAG);

const uint64_t ERR_COMP_NOT_NUM        = 1;
const uint64_t ERR_ARITH_NOT_NUM       = 2;
const uint64_t ERR_LOGIC_NOT_BOOL      = 3;
const uint64_t ERR_IF_NOT_BOOL         = 4;
const uint64_t ERR_OVERFLOW            = 5;
const uint64_t ERR_GET_NOT_TUPLE       = 6;
const uint64_t ERR_GET_LOW_INDEX       = 7;
const uint64_t ERR_GET_HIGH_INDEX      = 8;
const uint64_t ERR_GET_NOT_NUM         = 9;
const uint64_t ERR_NIL_DEREF           = 10;
const uint64_t ERR_OUT_OF_MEMORY       = 11;
const uint64_t ERR_SET_NOT_TUPLE       = 12;
const uint64_t ERR_SET_LOW_INDEX       = 13;
const uint64_t ERR_SET_NOT_NUM         = 14;
const uint64_t ERR_SET_HIGH_INDEX      = 15;
const uint64_t ERR_CALL_NOT_CLOSURE    = 16;
const uint64_t ERR_CALL_ARITY_ERR      = 17;
const uint64_t ERR_TUPLE_SIZE_MISMATCH = 18;
const uint64_t ERR_PATTERN_NOT_TUPLE   = 19;

typedef struct equal_cache {
  SNAKEVAL left;
  SNAKEVAL right;
} equal_cache;

int ensure_cache(equal_cache** p_cache, int last, int size, int needed) {
  int minneeded = last + needed;
  if (minneeded >= size) {
    int doubled = size * 2;
    int newsize = (doubled > minneeded) ? doubled : minneeded;
    equal_cache* newcache = (equal_cache*)realloc(*p_cache, newsize * sizeof(equal_cache));
    if (newcache != NULL) {
      *p_cache = newcache;
      return newsize;
    } else {
      fprintf(stderr, "Internal error while trying to compute equality\n");
      return 0;
    }
  }
  return size;
}

SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) {
  int size = 100;
  equal_cache* cache = (equal_cache*)calloc(size, sizeof(equal_cache));
  int cur = 0;
  int last = 1;
  SNAKEVAL ans = BOOL_TRUE;
  cache[cur].left = val1;
  cache[cur].right = val2;
  while (cur < last) {
    val1 = cache[cur].left;
    val2 = cache[cur].right;
    cur++;
    if (val1 == val2) { continue; }
    if (val1 == NIL || val2 == NIL) { ans = BOOL_FALSE; break; }
    int found_cached = -1;
    for (int i = 0; i < cur - 1; i++) {
      if (cache[i].left == val1 && cache[i].right == val2) {
        found_cached = i;
        break;
      }
    }
    if (found_cached > -1) { continue; }
    if ((val1 & TUPLE_TAG_MASK) == TUPLE_TAG && (val2 & TUPLE_TAG_MASK) == TUPLE_TAG) {
      uint64_t *tup1 = (uint64_t*)(val1 - TUPLE_TAG);
      uint64_t *tup2 = (uint64_t*)(val2 - TUPLE_TAG);
      if (tup1[0] != tup2[0]) { ans = BOOL_FALSE; break; }
      size = ensure_cache(&cache, last, size, (tup1[0] >> 1));
      if (size == 0) {
        free(cache);
        return BOOL_FALSE;
      }
      // we store the arity as a SNAKEVAL, so we must shift to use as loop bound
      for (int i = 1; i <= (tup1[0] >> 1); i++) {
        cache[last].left = tup1[i];
        cache[last].right = tup2[i];
        last++;
      }
      continue;
    }
    ans = BOOL_FALSE;
    break;
  }
  free(cache);
  return ans;
}

int tupleCounter = 0;
void printHelp(FILE *out, SNAKEVAL val) {
  if (val == NIL) {
    fprintf(out, "nil");
  }
  else if((val & NUM_TAG_MASK) == NUM_TAG) {
    fprintf(out, "%ld", ((int64_t)val) >> 1);
  }
  else if(val == BOOL_TRUE) {
    fprintf(out, "true");
  }
  else if(val == BOOL_FALSE) {
    fprintf(out, "false");
  }
  else if ((val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    fprintf(out, "<function>");
  }
  else if ((val & TUPLE_TAG_MASK) == TUPLE_TAG) {
    uint64_t* addr = (uint64_t*)(val - TUPLE_TAG);
    if ((*addr & 0x8000000000000000) != 0) {
      fprintf(out, "<cyclic tuple %d>", (int)(*addr & 0x7FFFFFFFFFFFFFFF));
      return;
    }
    if ((*addr & FWDPTR_TAG_MASK) == FWDPTR_TAG) {
      fprintf(out, "old tuple size is now forwarding pointer %#018lx", *addr);
      return;
    }
    int len = ((int)addr[0] >> 1);
    *(addr) = 0x8000000000000000 | (++tupleCounter);
    fprintf(out, "(");
    for (int i = 1; i <= len; i++) {
      if (i > 1) fprintf(out, ", ");
      printHelp(out, addr[i]);
    }
    if (len == 1) fprintf(out, ", ");
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = (len << 1);
  }
  else {
    fprintf(out, "Unknown value: %#018lx", val);
  }
}


SNAKEVAL printStack(SNAKEVAL val, uint64_t* rsp, uint64_t* rbp, int args) {
  printf("RSP: %#018lx\t==>  ", (uint64_t)rsp); fflush(stdout);
  printHelp(stdout, *rsp); fflush(stdout);
  printf("\nRBP: %#018lx\t==>  ", (uint64_t)rbp); fflush(stdout);
  printHelp(stdout, *rbp); fflush(stdout);
  printf("\n(difference: %ld)\n", (uint64_t)(rsp - rbp)); fflush(stdout);
  printf("Requested return val: %#018lx\t==> ", (uint64_t)val); fflush(stdout);
  printHelp(stdout, val); fflush(stdout);
  printf("\n"); fflush(stdout);
  printf("Num args: %d\n", args);

  uint64_t* origEsp = rsp;
  
  if (rsp > rbp) {
    printf("Error: RSP and RBP are not properly oriented\n"); fflush(stdout);
  } else {
    for (uint64_t* cur = rsp; cur < STACK_BOTTOM + 3; cur++) {
      if (cur == STACK_BOTTOM) {
        printf("BOT %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur); fflush(stdout);
      } else if (cur == rbp) {
        printf("RBP %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur); fflush(stdout);
      } else if (cur == origEsp) {
        printf("    %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur); fflush(stdout);
      } else if (cur == rbp + 1) {
        printf("    %#018lx: %#018lx\t==>  saved ret\n", (uint64_t)cur, *cur); fflush(stdout);
        rsp = rbp + 2;
        rbp = (uint64_t*)(*rbp);
      } else if (cur == STACK_BOTTOM + 2) {
        printf("    %#018lx: %#018lx\t==>  heap\n", (uint64_t)cur, *cur); fflush(stdout);
      } else {
        printf("    %#018lx: %#018lx\t==>  ", (uint64_t)cur, *cur); fflush(stdout);
        printHelp(stdout, *cur); fflush(stdout);
        printf("\n"); fflush(stdout);
      }
    }
  }
  return val;
}

SNAKEVAL input() {
  uint64_t ans;
  scanf("%ld", &ans);
  return ans << 1;
}

SNAKEVAL print(SNAKEVAL val) {
  printHelp(stdout, val);
  printf("\n");
  fflush(stdout);
  return val;
}

SNAKEVAL add_trigger(SNAKEVAL val) {
    fprintf(stderr, "Error: X86_64 does not support addTrigger\n");
}

SNAKEVAL alert_val(SNAKEVAL val) {
    fprintf(stderr, "Error: X86_64 does not support alert\n");
}

SNAKEVAL display_val(SNAKEVAL val) {
    fprintf(stderr, "Error: X86_64 does not support display\n");
}

SNAKEVAL big_bang(SNAKEVAL on_tick, SNAKEVAL render, SNAKEVAL value, SNAKEVAL should_world_end) {
    fprintf(stderr, "Error: X86_64 does not support bigBang\n");
}

void error(uint64_t code, SNAKEVAL val) {
  switch (code) {
  case ERR_COMP_NOT_NUM:
    fprintf(stderr, "Error: comparison expected a number, got "); printHelp(stderr, val);
    break;
  case ERR_ARITH_NOT_NUM:
    fprintf(stderr, "Error: arithmetic expected a number, got "); printHelp(stderr, val);
    break;
  case ERR_LOGIC_NOT_BOOL:
    fprintf(stderr, "Error: logic expected a boolean, got "); printHelp(stderr, val);
    break;
  case ERR_IF_NOT_BOOL:
    fprintf(stderr, "Error: if expected a boolean, got "); printHelp(stderr, val);
    break;
  case ERR_OVERFLOW:
    fprintf(stderr, "Error: Integer overflow, got "); printHelp(stderr, val);
    break;
  case ERR_GET_NOT_TUPLE:
    fprintf(stderr, "Error: get expected tuple, got "); printHelp(stderr, val);
    break;
  case ERR_GET_LOW_INDEX:
    fprintf(stderr, "Error: index too small to get, got "); printHelp(stderr, val);
    break;
  case ERR_GET_HIGH_INDEX:
    fprintf(stderr, "Error: index too large to get, got "); printHelp(stderr, val);
    break;
  case ERR_GET_NOT_NUM:
    fprintf(stderr, "Error: get expected numeric index, got "); printHelp(stderr, val);
    break;
  case ERR_NIL_DEREF:
    fprintf(stderr, "Error: tried to access component of nil\n");
    break;
  case ERR_OUT_OF_MEMORY:
    fprintf(stderr, "Error: out of memory\n");
    break;
  case ERR_SET_NOT_TUPLE:
    fprintf(stderr, "Error: set expected tuple, got "); printHelp(stderr, val);
    break;
  case ERR_SET_LOW_INDEX:
    fprintf(stderr, "Error: index too small to set, got "); printHelp(stderr, val);
    break;
  case ERR_SET_HIGH_INDEX:
    fprintf(stderr, "Error: index too large to set, got "); printHelp(stderr, val);
    break;
  case ERR_SET_NOT_NUM:
    fprintf(stderr, "Error: set expected numeric index, got "); printHelp(stderr, val);
    break;
  case ERR_CALL_NOT_CLOSURE:
    fprintf(stderr, "Error: tried to call a non-closure value, got "); printHelp(stderr, val);
    break;
  case ERR_CALL_ARITY_ERR:
    fprintf(stderr, "Error: arity mismatch in call");
    break;
  case ERR_TUPLE_SIZE_MISMATCH:
    fprintf(stderr, "Error: pattern expected a tuple of size "); printHelp(stderr, val);
    break;
  case ERR_PATTERN_NOT_TUPLE:
    fprintf(stderr, "Error: pattern expected a tuple, got "); printHelp(stderr, val);
    break;
  default:
    fprintf(stderr, "Error: Unknown error code: %ld, val: ", code); printHelp(stderr, val);
  }
  free(HEAP);
  exit(code);
}


/*
  Try to reserve the desired number of bytes of memory, and free garbage if
  needed.  Fail (and exit the program) if there is insufficient memory.  Does 
  not actually allocate the desired number of bytes of memory; the caller 
  will do that.

  Arguments:

    uint64_t* alloc_ptr - the current top of the heap (which we store in R15), where
                          the next allocation should occur, if possible
    uint64_t bytes_needed - the number of bytes of memory we want to allocate
                            (including padding)
    uint64_t* cur_frame - the base pointer of the topmost stack frame of our code
                          (i.e., RBP)
    uint64_t* cur_stack_top - the stack pointer of the topmost stack frame of our
                              code (i.e., RSP)

  Returns:
    The new top of the heap (i.e. the new value of R15) after garbage collection.  
    Does not actually allocate bytes_needed space.

  Side effect:
    Also updates HEAP and HEAP_END to point to the new location of the heap
*/
uint64_t* try_gc(uint64_t* alloc_ptr, uint64_t bytes_needed, uint64_t* cur_frame, uint64_t* cur_stack_top) {  
  uint64_t* new_heap = (uint64_t*)calloc(HEAP_SIZE + 15, sizeof(uint64_t));
  uint64_t* old_heap = HEAP;
  uint64_t* old_heap_end = HEAP_END;

  uint64_t* new_r15 = (uint64_t*)(((uint64_t)new_heap + 15) & ~0xF);
  uint64_t* new_heap_end = new_r15 + HEAP_SIZE;

  FROM_S = (uint64_t*)(((uint64_t)HEAP + 15) & ~0xF);
  FROM_E = HEAP_END;
  TO_S = new_r15;
  TO_E = new_heap_end;

  /* printf("FROM_S = %p, FROM_E = %p, TO_S = %p, TO_E = %p\n", FROM_S, FROM_E, TO_S, TO_E); */
  /* naive_print_heap(FROM_S, FROM_E); */
  /* printStack(BOOL_TRUE, cur_stack_top, cur_frame, 0); */

  // Abort early, if we can't allocate a new to-space
  if (new_heap == NULL) {
    fprintf(stderr, "Out of memory: could not allocate a new semispace for garbage collection\n");
    fflush(stderr);
    if (old_heap != NULL) free(old_heap);
    exit(ERR_OUT_OF_MEMORY);
  }
  
  new_r15 = gc(STACK_BOTTOM, cur_frame, cur_stack_top, FROM_S, HEAP_END, new_r15);

  // smarter_print_heap(FROM_S, FROM_E, TO_S, TO_E);
  HEAP = new_heap;
  HEAP_END = new_heap_end;
  free(old_heap);

  // Note: strict greater-than is correct here: if new_r15 + (bytes_needed / 8) == HEAP_END,
  // that does not mean we're *using* the byte at HEAP_END, but rather that it would be the
  // next free byte, which is still ok and not a heap-overflow.
  if (bytes_needed / sizeof(uint64_t) > HEAP_SIZE) {
    fprintf(stderr, "Allocation error: needed %ld words, but the heap is only %ld words\n",
            bytes_needed / sizeof(uint64_t), HEAP_SIZE);
    fflush(stderr);
    if (new_heap != NULL) free(new_heap);
    exit(ERR_OUT_OF_MEMORY);
  } else if((new_r15 + (bytes_needed / sizeof(uint64_t))) > HEAP_END) {
    fprintf(stderr, "Out of memory: needed %ld words, but only %ld remain after collection\n",
            bytes_needed / sizeof(uint64_t), (HEAP_END - new_r15));
    fflush(stderr);
    if (new_heap != NULL) free(new_heap);
    exit(ERR_OUT_OF_MEMORY);
  } else {
    /* fprintf(stderr, "new_r15 = %p\n", new_r15); */
    /* naive_print_heap(HEAP, HEAP_END); */
    return new_r15;
  }
}

// int main(int argc, char** argv) {
//   int size = 100000;
//   if (argc > 1) { size = atoi(argv[1]); }
//   if (size < 0 || size > 1000000) { size = 0; }
//   HEAP = calloc(size, sizeof (int));

//   SNAKEVAL result = our_code_starts_here(HEAP, size);
//   print(result);
//   free(HEAP);
//   return 0;
// }

int main(int argc, char** argv) {
  HEAP_SIZE = 100000;
  if (argc > 1) { HEAP_SIZE = atoi(argv[1]); }
  if (HEAP_SIZE < 0 || HEAP_SIZE > 1000000) { HEAP_SIZE = 0; }
  HEAP = (uint64_t*)calloc(HEAP_SIZE + 15, sizeof(uint64_t));

  uint64_t* aligned = (uint64_t*)(((uint64_t)HEAP + 15) & ~0xF);
  HEAP_END = aligned + HEAP_SIZE;
  /* printf("HEAP = %p, aligned = %p, HEAP_END = %p\n", HEAP, aligned, HEAP_END); */
  SNAKEVAL result = our_code_starts_here(aligned, HEAP_SIZE);
  /* smarter_print_heap(aligned, HEAP_END, TO_S, TO_E); */
  print(result);
  // naive_print_heap(aligned, HEAP_END);

  free(HEAP);
  return 0;
}