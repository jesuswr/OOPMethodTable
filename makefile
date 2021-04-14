all: test methodtable

test:
	ghc -fhpc Tests.hs MethodTable.hs

methodtable:
	ghc Main.hs -o TestMethodTable

clean:
	rm TestMethodTable Tests *.o *.hi 
	if test -s "Tests.tix"; then rm Tests.tix; fi
	if test -s ".hpc"; then rm -r .hpc; fi
	if test -s "hpc_index.html"; then rm *.html; fi