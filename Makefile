COMPILER = ocamlopt
OUTPUT = GTM
LINKPKG_CMD = -linkpkg -package
PACKAGES = yojson,xml-light,unix,cohttp-lwt-unix,cryptokit,str

COINBASE_DIR = coinbase
COINBASE_SRC = $(wildcard $(COINBASE_DIR)/*.mli $(COINBASE_DIR)/*.ml)

UTILITY_DIR = utility
UTILITY_SRC = $(wildcard $(UTILITYDIR)/*.mli $(UTILITY_DIR)/*.ml)

CORE_DIR = core
CORE_SRC = $(wildcard $(CORE_DIR)/*.mli $(CORE_DIR)/*.ml)

SRC = $(UTILITY_SRC) $(COINBASE_SRC) $(CORE_SRC)

all: build clean

build: 
	ocamlfind $(COMPILER) -o $(OUTPUT) $(LINKPKG_CMD) $(PACKAGES) -thread -I $(COINBASE_DIR) -I $(UTILITY_DIR) -I $(CORE_DIR) $(SRC)

clean: 
	rm _build -rf 
	mkdir _build
	mv $(COINBASE_DIR)/*.cmi $(COINBASE_DIR)/*cmx $(COINBASE_DIR)/*.o \
	$(UTILITY_DIR)/*.cmi $(UTILITY_DIR)/*cmx $(UTILITY_DIR)/*.o \
	$(CORE_DIR)/*.cmi $(CORE_DIR)/*cmx $(CORE_DIR)/*.o _build \
