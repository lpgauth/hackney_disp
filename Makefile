PROJECT=marina
REBAR=./rebar

all: deps compile
clean:
	@echo "Running rebar clean..."
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar update-deps..."
	@$(REBAR) update-deps

xref:
	@$(REBAR) skip_deps=true xref

.PHONY: clean compile deps xref
