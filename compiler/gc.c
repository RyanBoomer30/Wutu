#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "gc.h"

typedef uint64_t SNAKEVAL;

void printHelp(FILE* out, SNAKEVAL val);
extern uint64_t NUM_TAG_MASK;
extern uint64_t CLOSURE_TAG_MASK;
extern uint64_t FWDPTR_TAG_MASK;
extern uint64_t TUPLE_TAG_MASK;
extern uint64_t CLOSURE_TAG;
extern uint64_t TUPLE_TAG;
extern uint64_t FWDPTR_TAG;
extern uint64_t NIL;
extern uint64_t tupleCounter;
extern uint64_t* STACK_BOTTOM;
extern uint64_t* FROM_S;
extern uint64_t* FROM_E;
extern uint64_t* TO_S;
extern uint64_t* TO_E;

void naive_print_heap(uint64_t* heap, uint64_t* heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for(uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %ld/%p: %p (%ld)\n", i, (heap + i), (uint64_t*)(*(heap + i)), *(heap + i));
  }
}

// Implement the functions below

void smarter_print_heap(uint64_t* from_start, uint64_t* from_end, uint64_t* to_start, uint64_t* to_end) {
  // Print out the entire heap (both semispaces), and
  // try to print values readably when possible
  printf("Smart print of old heap %p to %p\n", from_start, from_end);
  for (uint64_t* cur_word = from_start; cur_word < from_end; cur_word++) {
    printf("address %p |-> ", cur_word);
    printHelp(stdout, *cur_word);
    printf("\n");
  }

  printf("Smart print of new heap %p to %p\n", to_start, to_end);
  for (uint64_t* cur_word = to_start; cur_word < to_end; cur_word++) {
    printf("address %p |-> ", cur_word);
    printHelp(stdout, *cur_word);
    printf("\n");
  }
}

/*
  Copies a SNAKEVAL from the given address to the new heap, 
  but only if the value is heap-allocated and needs copying.

  Arguments:
    garter_val_addr: the *address* of some SNAKEVAL, which contains a tagged word.
                     It may or may not be a pointer to a heap-allocated value...
    heap_top: the location at which to begin copying, if any copying is needed

  Return value:
    The new top of the heap, at which to continue allocations

  Side effects:
    If the data needed to be copied, then this replaces the value at its old location 
    with a forwarding pointer to its new location.
    Also, updates the SNAKEVAL at snake_val_addr to point to the new location,
    if something had to be copied over.
 */
uint64_t* copy_if_needed(SNAKEVAL* snake_val_addr, uint64_t* heap_top) {
  SNAKEVAL snake_val = *snake_val_addr;
  
  // check if snake_val is heap allocated (either a tuple or closure)
  if ((snake_val & TUPLE_TAG_MASK) == TUPLE_TAG) {
    // tuple case
    uint64_t* tuple_ptr = (uint64_t*)(snake_val - TUPLE_TAG);
    
    // nil in our language is a NULL pointer tagged as a tuple, so check that
    if (tuple_ptr == NULL) {
      return heap_top;
    }
    
    if ((*tuple_ptr & FWDPTR_TAG_MASK) == FWDPTR_TAG) {
      // if we have already copied the tuple, just follow the forwarding
      // pointer, making sure to tag it appropriately
      *snake_val_addr = (*tuple_ptr - FWDPTR_TAG + TUPLE_TAG);
      return heap_top;
    }

    // otherwise, we have to copy the tuple over...
    int64_t num_entries = *tuple_ptr >> 1;
    // make sure to add the extra one for the size...
    int64_t num_no_padding = (num_entries + 1);
    // and copy over padding to ensure alignment
    int64_t num_to_copy = num_no_padding + (num_no_padding % 2);
    memcpy(heap_top, tuple_ptr, num_to_copy * 8);
    
    // after copy, fix up snake_val_addr and replace with fwd pointer
    *snake_val_addr = ((uint64_t) heap_top + TUPLE_TAG);
    *tuple_ptr = ((uint64_t) heap_top) + FWDPTR_TAG;
    
    return heap_top + num_to_copy;
    
  } else if ((snake_val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    // closure case 
    uint64_t* closure_ptr = (uint64_t*)(snake_val - CLOSURE_TAG);

    if ((*closure_ptr & FWDPTR_TAG_MASK) == FWDPTR_TAG) {
      // if we have already copied, just follow the forwarding pointer
      *snake_val_addr = (*closure_ptr - FWDPTR_TAG + CLOSURE_TAG);
      return heap_top;
    }

    // otherwise, we have to copy the closure over, where the number of closed-over
    // variables is stored in the third word of the closure
    int64_t num_closed_over = *(closure_ptr + 2) >> 1;
    // make sure to add the extra one for the size...
    int64_t num_no_padding = (num_closed_over + 3);
    // and copy over padding to ensure alignment
    int64_t num_to_copy = num_no_padding + (num_no_padding % 2);
    memcpy(heap_top, closure_ptr, num_to_copy * 8);
    
    // after copy, fix up snake_val_addr and replace with fwd pointer
    *snake_val_addr = ((uint64_t) heap_top + CLOSURE_TAG);
    *closure_ptr = ((uint64_t) heap_top) + FWDPTR_TAG;
    
    return heap_top + num_to_copy;
  }

  // everything else is not a heap allocated value -> ignore them
  return heap_top;
}

/*
  Implements Cheney's garbage collection algorithm.

  Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost Garter frame
    top_frame: the base pointer of the topmost Garter stack frame
    top_stack: the current stack pointer of the topmost Garter stack frame
    from_start and from_end: bookend the from-space of memory that is being compacted
    to_start: the beginning of the to-space of memory

  Returns:
    The new location within to_start at which to allocate new data
 */
uint64_t* gc(uint64_t* bottom_frame, uint64_t* top_frame, uint64_t* top_stack, uint64_t* from_start, uint64_t* from_end, uint64_t* to_start) {
  uint64_t* old_to_start = to_start;
  uint64_t* old_top_frame = top_frame;
  do {
    // maybe needs a +1 somewhere
    for (uint64_t* cur_word = top_stack; cur_word < top_frame; cur_word++) {
      // copies over all of the roots, updating as appropriate
      to_start = copy_if_needed(cur_word, to_start);
    }
    /* Shift to next stack frame:
     * [top_frame] points to the saved RBP, which is the RBP of the next stack frame,
     * [top_frame + 8] is the return address, and
     * [top_frame + 16] is therefore the next frame's stack-top
     */
    top_stack = top_frame + 2;
    old_top_frame = top_frame;
    top_frame = (uint64_t*)(*top_frame);
  } while (old_top_frame < bottom_frame); // Use the old stack frame to decide if there's more GC'ing to do

  // to_start is the current top of the new heap
  // during the loop, to_start will be updated.
  // we don't want to check when = to_start, since that will be the first word
  // that is available for next allocation (and therefore will contain random stuff)
  for (uint64_t* cur_heap_word = old_to_start; cur_heap_word < to_start; cur_heap_word++) {
    // cur_heap_word is a SNAKEVAL (or a code pointer, which will be even -> interpreted as number)
    to_start = copy_if_needed(cur_heap_word, to_start);
  }

  // after copying and GC'ing all the stack frames, return the new allocation starting point
  return to_start;       
}

