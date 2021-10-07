COMPILER = ocamlopt
OUTPUT = output
LINKPKG_CMD = -linkpkg -package
PACKAGES = yojson,xml-light,unix,cohttp-lwt-unix,cryptokit,str

COINBASE_DIR = coinbase
COINBASE_SRC = $(wildcard $(COINBASE_DIR)/*.mli $(COINBASE_DIR)/*.ml)

UTILITY_DIR = utility
UTILITY_SRC = $(wildcard $(UTILITYDIR)/*.mli $(UTILITY_DIR)/*.ml)

TEST_DIR = test
TEST_SRC = $(wildcard $(TEST_DIR)/*.ml)

SRC = $(UTILITY_SRC) $(COINBASE_SRC) $(TEST_SRC)

clean: 
	-rm _build -rf 
	mkdir _build
	mv $(COINBASE_DIR)/*.cmi $(COINBASE_DIR)/*cmx $(COINBASE_DIR)/*.o \
	$(UTILITY_DIR)/*.cmi $(UTILITY_DIR)/*cmx $(UTILITY_DIR)/*.o \
	$(TEST_DIR)/*.cmi $(TEST_DIR)/*cmx $(TEST_DIR)/*.o _build \

build:
	eval $(opam config env)
	-ocamlfind $(COMPILER) -o $(OUTPUT) $(LINKPKG_CMD) $(PACKAGES) -thread -I $(COINBASE_DIR) -I $(UTILITY_DIR) -I $(TEST_DIR) $(SRC)