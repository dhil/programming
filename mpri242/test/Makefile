.PHONY: test clean user

JOUJOU=../src/joujou

test:
	@for i in good/*.f ; do \
          rm -f $${i%f}out $${i%f}err $${i%f}ff ; \
	  if $(JOUJOU) --typecheck $$i >$${i%f}out 2>$${i%f}err ; then \
	    if grep Warning $${i%f}err ; then \
	      echo $$i: typechecking the source program produced a warning -- this is WRONG! ; \
	    elif $(JOUJOU) --translate $$i >$${i%f}ff 2>$${i%f}err ; then \
	      if $(JOUJOU) --typecheck $${i%f}ff >$${i%f}out 2>$${i%f}err ; then \
		echo $$i: successful. ; \
              else \
	        echo $$i: typechecking the translated program FAILED. ; \
	      fi \
            else \
	      echo $$i: the translation FAILED. ; \
	    fi \
	  else \
	    echo $$i: typechecking the source program FAILED. ; \
	  fi \
	done > good/log
	@for i in warnings/*.f ; do \
          rm -f $${i%f}out $${i%f}err $${i%f}ff ; \
	  if $(JOUJOU) --typecheck $$i >$${i%f}out 2>$${i%f}err ; then \
	    if grep Warning $${i%f}err ; then \
	      echo $$i: successful. ; \
	    else \
	      echo $$i: typechecking did not produce a warning -- this is WRONG! ; \
	    fi \
          else \
	    echo $$i: typechecking FAILED. ; \
	  fi \
	done > warnings/log
	@for i in bad/*.f ; do \
	  rm -f $${i%f}out $${i%f}err ; \
	  if $(JOUJOU) --typecheck $$i >$${i%f}out 2>$${i%f}err ; then \
	    echo $$i: typechecking succeeded -- this is WRONG! ; \
	  else \
	    if grep "Fatal error: exception" $${i%f}err ; then \
	      echo $$i: typechecking the source program FAILED with an exception. ; \
	    else \
	      echo $$i: successful. ; \
	    fi \
	  fi \
	done > bad/log
	@cat good/log warnings/log bad/log | grep successful
	@grep FAILED good/log warnings/log bad/log || true
	@grep WRONG  good/log warnings/log bad/log || true
	@grep "Fatal error: exception" {good,warnings,bad}/*.err || true

user:
	$(MAKE) JOUJOU=../2009/rendus/$(USER)/src/joujou test

clean:
	rm -f {bad,warnings,good}/*.{out,err,ff}
	rm -f {bad,warnings,good}/*~ *~
	rm -f {bad,warnings,good}/log
