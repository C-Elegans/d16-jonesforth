forth.bin: forth.o numeric.o
	d16-ld -o $@ $^
%.o: %.d16
	d16 -o $@ $<
%.d16: %.m4
	   m4 $< > $@

