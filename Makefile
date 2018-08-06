all: forth.bin
preforth.bin: forth.o numeric.o data.o
	d16-ld -o $@ -l lkr.lst $^ 
%.o: %.d16
	d16 -o $@ $<
%.d16: %.m4
	   m4 $< > $@

forth.bin: preforth.bin words.f
	cat words.f | d16-jit -d $@ -e 0xf000 $<
run: forth.bin
	d16-jit $<

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.bin

