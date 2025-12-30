# Makefile for Owl lexer demo (C17)

# Compiler and flags
CC      ?= cc
CFLAGS  ?= -std=c17 -Wall -Wextra -Wpedantic -O2
LDFLAGS ?=
BIN     := owl-lexer

SRC     := lexer.c parser.c main.c
# Parser demo binary and sources
BIN_PARSER := owl-parser
SRC_PARSER := lexer.c parser.c main_parse_demo.c
OBJ_PARSER := $(patsubst %.c,build/%.o,$(SRC_PARSER))
OBJ     := $(patsubst %.c,build/%.o,$(SRC))
# Transpiler demo binary and sources
BIN_TRANSPILE := owl-transpiler
SRC_TRANSPILE := lexer.c parser.c transpile.c main_transpile_demo.c
OBJ_TRANSPILE := $(patsubst %.c,build/%.o,$(SRC_TRANSPILE))

.PHONY: all clean run run-parser run-transpiler parse-demo

BIN_DIR := bin
BUILD_DIR := build
all: $(BIN_DIR)/$(BIN) $(BIN_DIR)/$(BIN_PARSER) $(BIN_DIR)/$(BIN_TRANSPILE)

$(BIN_DIR)/$(BIN): $(OBJ)
	mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -o $@ $(OBJ) $(LDFLAGS)

$(BIN_DIR)/$(BIN_PARSER): $(OBJ_PARSER)
	mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -o $@ $(OBJ_PARSER) $(LDFLAGS)

$(BIN_DIR)/$(BIN_TRANSPILE): $(OBJ_TRANSPILE)
	mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -o $@ $(OBJ_TRANSPILE) $(LDFLAGS)

build/%.o: %.c lexer.h parser.h transpile.h owl_rt.h
	mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

run: $(BIN_DIR)/$(BIN)
	./$(BIN_DIR)/$(BIN)

run-parser: $(BIN_DIR)/$(BIN_PARSER)
	./$(BIN_DIR)/$(BIN_PARSER)

run-transpiler: $(BIN_DIR)/$(BIN_TRANSPILE)
	./$(BIN_DIR)/$(BIN_TRANSPILE)

parse-demo: $(BIN_DIR)/$(BIN)
	@echo "Building $(BIN) with parser sources linked"
	@echo "Note: main.c currently prints lexer tokens; parser is linked and ready for a dedicated demo."

clean:
	rm -f $(OBJ) $(OBJ_PARSER) $(OBJ_TRANSPILE) $(BIN_DIR)/$(BIN) $(BIN_DIR)/$(BIN_PARSER) $(BIN_DIR)/$(BIN_TRANSPILE)
