MODULE_NAME=complex_sequence_2

qaic:
	qaic -st -I../../inc $(SYSTEM_CFLAGS) inc/$(MODULE_NAME).idl

# Just HACK foir now till we fix QAIC to do this correctly

TEST_SRC = src/$(MODULE_NAME)_main.c src/$(MODULE_NAME)_test.c ../util/util.c
SKEL_SRC = $(MODULE_NAME)_skel.c src/$(MODULE_NAME)_imp.c
STUB_SRC = $(MODULE_NAME)_stub.c

SKEL_LIB=lib$(MODULE_NAME)_skel.so
STUB_LIB=lib$(MODULE_NAME)_stub.so
STUB_LINK_LIB=$(MODULE_NAME)_stub
TEST_APP=$(MODULE_NAME)_test

skel: CFLAGS = @SKEL_CFLAGS@ -I. -Iinc -I../inc -I../../inc -I../util
skel: LDFLAGS = $(SKEL_LDFLAGS) -soname $(SKEL_LIB)

stub: CFLAGS = @STUB_CFLAGS@ -I. -Iinc -I../inc -I../../inc -I../util
stub: LDFLAGS = @STUB_LDFLAGS@ -soname $(STUB_LIB)


test: CFLAGS = @TEST_CFLAGS@ -I. -Iinc -I../inc -I../../inc -I../util
test: LDFLAGS = @TEST_LDFLAGS@ --target=aarch64-linux-gnu -lbsd -l $(STUB_LINK_LIB) -L $(CURDIR)

SKEL_OBJ = $(SKEL_SRC:.c=.o)

STUB_OBJ = $(STUB_SRC:.c=.o)

TEST_OBJ = $(TEST_SRC:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

all: qaic skel stub test

skel: $(SKEL_OBJ)
	$(LD) $(LDFLAGS) -o $(SKEL_LIB) $^

stub: $(STUB_OBJ)
	$(LD) $(LDFLAGS) -o $(STUB_LIB) $^

test: $(TEST_OBJ)
	$(CC) $(LDFLAGS) -o $(TEST_APP) $^

install-exec-hook:
	$(INSTALL) -d $(DESTDIR)$(libdir)
	$(INSTALL_PROGRAM) $(SKEL_LIB) $(DESTDIR)$(libdir)/$(SKEL_LIB)

clean-local:
	rm -f *skel.c *stub.c *.o $(STUB_LIB) $(SKEL_LIB) $(TEST_APP)
