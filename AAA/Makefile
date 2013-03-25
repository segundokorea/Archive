all: aac aas

libpegged:
	cd pegged && make

common:
	dmd -lib $(DFLAGS) common.d

aac: libpegged common
	dmd pegged/libpegged.a -Ipegged common.a $(DFLAGS) aac.d

aas: common
	dmd common.a $(DFLAGS) aas.d

clean:
	rm -f aac aas *.aap *.o *.a pegged/*.a pegged/regenerate
