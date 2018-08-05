forth.bin: forth.o numeric.o data.o
	d16-ld -o $@ -l lkr.lst $^ 
%.o: %.d16
	d16 -o $@ $<
%.d16: %.m4
	   m4 $< > $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.bin

