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
APPS = erts kernel stdlib compiler crypto syntax_tools parsetools

dialyzer: build $(PLT)
	dialyzer --plt $(PLT) ./ebin --get_warnings

$(PLT):
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS)

# eunit
eunit: build
	erl -noshell -pa ebin -eval "eunit:test(epc, [verbose])" -s init stop
