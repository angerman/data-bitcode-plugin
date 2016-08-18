GHC=$(HOME)/opt/lib/ghc-8.0.1

test-link:
	cc \
	-I$(GHC)/include \
  -L$(GHC)/base-4.9.0.0 \
	-L$(GHC)/integer-gmp-1.0.0.1 \
	-L$(GHC)/ghc-prim-0.5.0.0 \
	-L$(GHC)/rts \
	-lHSbase-4.9.0.0 \
	-lHSinteger-gmp-1.0.0.1 \
	-lHSghc-prim-0.5.0.0 \
	-lHSrts \
	-lCffi \
	-liconv \
	-lgmp \
	-lm \
	-ldl \
	stub.c \
	Test.ll.opt.bc


Test.ll.opt.bc: Test.ll.bc
	opt -mem2reg < Test.ll.bc > Test.ll.opt.bc

Test.ll.o: Test.ll.bc
	cc -o test.ll.o -c Test.ll.bc

Test.ll.opt.o: Test.ll.opt.bc
	cc -o Test.ll.opt.o -c Test.ll.opt.bc

stub.o:
	cc -I$(GHC)/include \
	-o stub.o -c stub.c

Test.bc: Test.ll.o stub.o
	cc \
	-L$(GHC)/base-4.9.0.0 \
	-L$(GHC)/integer-gmp-1.0.0.1 \
	-L$(GHC)/ghc-prim-0.5.0.0 \
	-L$(GHC)/rts \
	-lHSbase-4.9.0.0 \
	-lHSinteger-gmp-1.0.0.1 \
	-lHSghc-prim-0.5.0.0 \
	-lHSrts \
	-lCffi \
	-liconv \
	-lgmp \
	-lm \
	-ldl \
	-o Test.bc \
	Test.ll.o \
	stub.o

Test.bc.opt: Test.ll.opt.o stub.o
	cc \
	-L$(GHC)/base-4.9.0.0 \
	-L$(GHC)/integer-gmp-1.0.0.1 \
	-L$(GHC)/ghc-prim-0.5.0.0 \
	-L$(GHC)/rts \
	-lHSbase-4.9.0.0 \
	-lHSinteger-gmp-1.0.0.1 \
	-lHSghc-prim-0.5.0.0 \
	-lHSrts \
	-lCffi \
	-liconv \
	-lgmp \
	-lm \
	-ldl \
	-o Test.bc.opt \
	Test.ll.opt.o \
	stub.o

.PHONY: clean
clean:
	rm -fR tmp/*
	rm *.hi
	rm *.bc
	rm *.ll
	rm *.o
