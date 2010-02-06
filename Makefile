PKG_NAME = slerl

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
PARSERS = $(wildcart $(PRIV_DIR)/*.yrl)

TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

ERLC_INCLUDES = -I $(INCLUDE_DIR)
ERL_EBINS = -pa $(EBIN_DIR)

ERLC = erlc
ERLC_OPTS = $(ERLC_INCLUDES) -o $(EBIN_DIR) -Wall +debug_info

ERL_CMD=erl \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	$(ERL_EBINS)

all: $(TARGETS)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -s ${PKG_NAME}_app launch

test: run_prereqs
	$(ERL_CMD) -noshell -s slerl_login test -s init stop

parser: 
	erl -noshell -eval 'yecc:yecc("priv/messages.yrl", "src/slerl_message_template_parser.erl").' -s init stop

stop:
	erl_call -a '$(PKG_NAME)_app stop_and_halt []' -sname $(PKG_NAME)

clean: cleanlog
	rm -f $(TARGETS)
	rm -f $(EBIN_DIR)/*.beam

cleanlog:
	rm -f auth.log report.log sasl_err.log
	rm -f *.access

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<

