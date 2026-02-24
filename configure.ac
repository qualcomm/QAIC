AC_INIT([HexagonSamples], [1.0], [srinivas.kandagatla@oss.qualcomm.com])
AM_INIT_AUTOMAKE([foreign -Wall])
AC_CANONICAL_HOST
AC_PROG_CC
case "$host_cpu" in
  aarch64*)
    ARCHINC="/usr/include/aarch64-linux-gnu"
    ;;
  x86_64*)
    ARCHINC="/usr/include/x86-linux-gnu"
    ;;
  *)
    ARCHINC="/usr/include"
    ;;
esac
AC_SUBST(ARCHINC)

# Compiler and linker
CC=clang
LD=ld.lld
AC_SUBST(CC)
AC_SUBST(LD)

AM_CFLAGS="-Wall -O2 -fPIC -Werror"
AC_SUBST(AM_CFLAGS)

#Command to generate skel and stub
QAIC_GENERATE="qaic -I ../inc"
AC_SUBST([QAIC_GENERATE])

# System include flags
SYSTEM_CFLAGS="-I/usr/include -I/usr/local/include -I/usr/local/include/fastrpc/inc -I../inc"
AC_SUBST(SYSTEM_CFLAGS)

SKEL_CFLAGS="-fPIC -O2 --target=hexagon -Wall -Werror -Iinc $SYSTEM_CFLAGS -I $ARCHINC"
SKEL_LDFLAGS="-shared -z separate-loadable-segments"
AC_SUBST([SKEL_CFLAGS])
AC_SUBST([SKEL_LDFLAGS])

STUB_CFLAGS="-fPIC -O2 --target=aarch64-linux-gnu -Wall -Werror -Iinc $SYSTEM_CFLAGS -I $ARCHINC -mno-outline-atomics"
STUB_LDFLAGS=" -shared"
AC_SUBST([STUB_CFLAGS])
AC_SUBST([STUB_LDFLAGS])

TEST_CFLAGS="-fPIC -O2 --target=aarch64-linux-gnu -Wall -Werror -Iinc $SYSTEM_CFLAGS -I $ARCHINC"
TEST_LDFLAGS="-lcdsprpc -L /usr/local/lib -L /local/mnt/workspace/suraj/new4/fastrpc/src/.libs"
AC_SUBST(TEST_CFLAGS)
AC_SUBST(TEST_LDFLAGS)


AC_CONFIG_FILES([
  Makefile
  testcases/calculator/Makefile
  testcases/array/Makefile
  testcases/buffer_argument_testcase/Makefile
  testcases/complex_seq/Makefile
  testcases/complex_sequence_1/Makefile
  testcases/complex_sequence_2/Makefile
  testcases/complex_struct/Makefile
  testcases/include_idl_1/Makefile
  testcases/include_idl_2/Makefile
  testcases/include_idl_3/Makefile
  testcases/qaic_string/Makefile
  testcases/qaic_testcase/Makefile
  testcases/qaic_wstring/Makefile
  testcases/test_mt_31_methods/Makefile
])
AC_OUTPUT
