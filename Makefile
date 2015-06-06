all: erlang

erlang: hello.beam wx_glfont.beam

%.beam: %.erl
	@erlc $<

run: all
	erl -setcookie $(COOKIE) -sname gui -s hello

clean:
	@rm -f *.beam erl_crash.dump *~
