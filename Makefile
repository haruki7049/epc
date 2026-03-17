all: build

# Build
build:
	erl -make

b: build

# Clean
clean:
	rm -f ebin/*.beam

c: clean

# Formatters by treefmt-nix
format:
	treefmt

f: format
fmt: format

# Dialyzer
PLT = .epc_dialyzer.plt
APPS = erts kernel stdlib compiler crypto syntax_tools parsetools eunit

dialyzer: build $(PLT)
	dialyzer --plt $(PLT) ./ebin --get_warnings

$(PLT):
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS)

# eunit
eunit: build
	erl -noshell -pa ebin -eval "eunit:test(epc, [verbose])" -s init stop

# common_test
common_test: build
	mkdir -p logs
	ct_run -pa ebin -dir test -logdir logs -no_auto_compile

# edoc
edoc: build
	erl -noshell -pa ebin -eval "edoc:application(epc, \"./src\", [{dir, "docs"}])" -s init stop
