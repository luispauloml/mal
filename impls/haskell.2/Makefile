SRCS = step0_repl.hs step1_read_print.hs
OTHER_SRCS = Types.hs Reader.hs Printer.hs
BINS = $(SRCS:%.hs=%)
ghc_flags = -Wall

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.hs $(OTHER_SRCS)
	ghc ${ghc_flags} --make $< -o $@

clean:
	rm -f $(BINS) mal *.hi *.o
