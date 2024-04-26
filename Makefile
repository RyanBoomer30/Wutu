SNAKE_EXT= wutu
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FLAGS=-mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho64
  CLANG_FLAGS=-mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer -arch x86_64
endif
endif

PKGS=ounit2,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind -cflag -annot -ocamlyacc 'ocamlyacc -v'

COMPDIR=compiler
TESTDIR=tests

main: $(COMPDIR)/*.ml $(COMPDIR)/parser.mly $(COMPDIR)/lexer.mll
	$(BUILD) -package $(PKGS) $(COMPDIR)/main.native
	mv main.native main

test: main $(TESTDIR)/*.ml
	$(BUILD) -I compiler -package $(PKGS) $(TESTDIR)/test.native
	mv test.native test

## SERVER ##

.PHONY: serve
serve:
	dune exec ./server.exe

playground/%.wasm: playground/%.wat
	wat2wasm $< -o $@ --enable-tail-call


## OCAML TESTS ##

# x86_64
tests/output/%.run: tests/output/%.o $(COMPDIR)/main.c $(COMPDIR)/gc.c
	clang $(CLANG_FLAGS) -o $@ $(COMPDIR)/gc.c $(COMPDIR)/main.c $<

tests/output/%.o: tests/output/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: tests/output/%.s
tests/output/%.s: tests/input/%.$(SNAKE_EXT) main
	./main $< > $@


# Wasm
tests/output/%.wasm: tests/output/%.wat
	wat2wasm $< -o $@ --enable-tail-call

.PRECIOUS: tests/output/%.wat
tests/output/%.wat: tests/input/%.$(SNAKE_EXT) main
	./main $< > $@ -wasm


## FILE TESTS ##

# x86_64
tests/output/do_pass/%.run: tests/output/do_pass/%.o $(COMPDIR)/main.c $(COMPDIR)/gc.c
	clang $(CLANG_FLAGS) -o $@ $(COMPDIR)/gc.c $(COMPDIR)/main.c $<

tests/output/do_pass/%.o: tests/output/do_pass/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: tests/output/do_pass/%.s
tests/output/do_pass/%.s: tests/input/do_pass/%.$(SNAKE_EXT) main
	./main $< > $@


tests/output/dont_pass/%.run: tests/output/dont_pass/%.o $(COMPDIR)/main.c $(COMPDIR)/gc.c
	clang -g $(CLANG_FLAGS) -o $@ $(COMPDIR)/gc.c $(COMPDIR)/main.c $<

tests/output/dont_pass/%.o: tests/output/dont_pass/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: tests/output/dont_pass/%.s
tests/output/dont_pass/%.s: tests/input/dont_pass/%.$(SNAKE_EXT) main
	./main $< > $@


tests/output/do_err/%.run: tests/output/do_err/%.o $(COMPDIR)/main.c $(COMPDIR)/gc.c
	clang $(CLANG_FLAGS) -o $@ $(COMPDIR)/gc.c $(COMPDIR)/main.c $<

tests/output/do_err/%.o: tests/output/do_err/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: tests/output/do_err/%.s
tests/output/do_err/%.s: tests/input/do_err/%.$(SNAKE_EXT) main
	./main $< > $@


tests/output/dont_err/%.run: tests/output/dont_err/%.o $(COMPDIR)/main.c $(COMPDIR)/gc.c
	clang -g $(CLANG_FLAGS) -o $@ $(COMPDIR)/gc.c $(COMPDIR)/main.c $<

tests/output/dont_err/%.o: tests/output/dont_err/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: tests/output/dont_err/%.s
tests/output/dont_err/%.s: tests/input/dont_err/%.$(SNAKE_EXT) main
	./main $< > $@

# gctest.o: gctest.c gc.h
# 	gcc gctest.c -m64 -c -g -o gctest.o

# gc.o: gc.c gc.h
# 	gcc gc.c -m64 -c -g -o gc.o

# cutest-1.5/CuTest.o: cutest-1.5/CuTest.c cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/CuTest.c -c -g -o cutest-1.5/CuTest.o

# gctest: gctest.o gc.c cutest-1.5/CuTest.o cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/AllTests.c cutest-1.5/CuTest.o gctest.o gc.c -o gctest


clean:
	rm -rf tests/output/*.o tests/output/*.s tests/output/*.dSYM tests/output/*.run tests/output/*.wasm tests/output/*.wat *.log *.o
	rm -rf tests/output/*/*.o tests/output/*/*.s tests/output/*/*.dSYM tests/output/*/*.run tests/output/*/*.wasm tests/output/*/*.wat
	rm -rf _build/
	rm -f main test