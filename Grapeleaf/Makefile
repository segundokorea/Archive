target  = Grapeleaf
objects = GLSynapse.o GLNeuron.o GLPattern.o
options = -lobjc -fobjc-gc -std=c99 -framework Foundation -g -O2 -Wall -Werror -pedantic

$(target): $(objects)
	cc $(options) -o $(target) $(objects) Grapeleaf.m

%.o: %.m
	cc -std=c99 -fobjc-gc -c -o $@ $^

Clean:
	rm -f $(target) $(objects)

Test:
	make Clean; echo; make; echo; ./$(target)