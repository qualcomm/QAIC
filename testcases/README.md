
# QAIC Testcases

## Overview

This directory contains testcases to test QAIC binary. These test cases validate different data types supported by QAIC. Each test case validates QAIC using corresponding IDL file and associated test driver and implementation files written in C.

*   **IDL files** define the interface between the application processor and Qualcomm Hexagon DSP.
*   **C source files** implement the corresponding service logic and test executables.

The build system uses a standard **GNU Autotools** workflow (`autoreconf → configure → make`) and is validated with the **LLVM/Clang** toolchain.

Supports **x86_64 host cross-compilation to aarch64** provided prerequisites and dependencies mentioned below are met.


## Prerequisites

### Toolchain

The following tools are required to build the testcases:

*   **LLVM/Clang** (version 14 or newer recommended)
*   **GNU Autotools**
    *   Autoconf 2.71
    *   Automake 1.16.5
    *   Libtool 2.4.6


### Libraries and headers

When building on an **x86\_64 host** and targeting **aarch64 (arm64)**, ensure the following are available:

*   **FastRPC cross‑build libraries** and headers
*   Runtime dependencies:
    *   `libyaml`
    *   `libmd`
    *   `libbsd`
    *   `libcdsprpc`

For reference, see:

*   **FastRPC build & installation documentation:**
    <https://github.com/qualcomm/fastrpc/tree/development?tab=readme-ov-file#build--installation>
*   **FastRPC cross‑compile documentation:**
    <https://github.com/qualcomm/fastrpc/blob/development/README.md#steps-to-cross-compile-the-project-on-ubuntu>


## Configuration steps

From the repository root:

```bash
# Generate Autotools files
autoreconf -fi
```

### Build‑time requirements

*   Ensure the FastRPC header files are installed and the path is included and also library paths are provided during configuration based on your environment.
*   `libcdsprpc` built for the **target architecture**.
*   During cross‑compilation, ensure that `CC`, `CFLAGS`, and `LDFLAGS` point to the correct target sysroot and libraries.

### Runtime requirements (on target)

*   `libcdsprpc.so` must be available in the runtime library search path.

If FastRPC is built from source using libtool, the shared library is typically generated under:

    <FASTRPC_SRC>/src/.libs/

### Configure with FastRPC library path

Set `FASTRPC_SRC` to the FastRPC source directory and pass the library path explicitly during configuration:

    FASTRPC_SRC=<path-to-fastrpc-shared-library>

```bash
TEST_LDFLAGS="-L$FASTRPC_SRC/src/.libs -lcdsprpc" ./configure
```

## Build

```bash
# Build (From the testcases root)
make
```

Each testcase build produces:

*   A test executable
*   A FastRPC **skeleton** shared library
*   A FastRPC **stub** shared library


## Runtime layout & deployment

To run a testcase on an **aarch64 target device**, copy the following files to the target.
Example shown for the `array` testcase:

*   `array_test` — test executable
*   `libarray_skel.so` — FastRPC service skeleton
*   `libarray_stub.so` — FastRPC client stub
*   `libbsd.so.0`
*   `libmd.so`
*   `libmd.so.0`

The libraries `libbsd.so.0`, `libmd.so`, and `libmd.so.0` only need to be copied if they are not already present on the target device. When required, place them in a standard library path on the target, such as `/usr/lib/`, to ensure they are available at runtime.

### Deploy to target

Use `adb` (or an equivalent mechanism) to copy the binaries and libraries to the target, for example:

```bash
adb push array_test /usr/bin/
adb push libarray_*.so /usr/lib/
```

### Run on target

```bash
export LD_LIBRARY_PATH=/usr/lib/:$LD_LIBRARY_PATH
cd /usr/bin
./array_test -d 3 -U 1
```

## Notes & tips

*   **Clean builds:**
    If you need to rebuild from scratch, use:
    ```bash
    make distclean
    ```
*   **Cross‑environment validation:**
    When cross‑compiling, ensure that `LD_LIBRARY_PATH` on the target resolves **only target‑architecture libraries**.
*   **PD support:**
    These testcases are **not supported on signed PDs**.
*   **Validation status:**
    Execution has been validated **only on unsigned PDs**.


## Troubleshooting

*   **Linker not found:**
    Ensure LLVM `lld` is installed and available in `PATH`.
    Example:
    ```bash
    which ld.lld
    ```
    Adjust `LD=ld.lld` if required.

*   **Missing runtime libraries:**
    If the test reports missing `libbsd.so.0`, `libmd.so`, or `libmd.so.0`, verify that these libraries exist on the target and are included in `LD_LIBRARY_PATH`.

*   **Cross‑compile failures:**
    Confirm that:
    *   FastRPC cross libraries are installed
    *   The correct `--target` and `sysroot` are used
    *   Include and library search paths match the target environment

Refer to the FastRPC README for authoritative cross‑compile guidance.


## License

This project uses the **BSD‑3‑Clause‑Clear** license.

All source files must include the appropriate SPDX identifier:

*   **C / C headers**
    ```c
    /* SPDX-License-Identifier: BSD-3-Clause-Clear */
    ```
*   **IDL files**
    ```idl
    // SPDX-License-Identifier: BSD-3-Clause-Clear
    ```


## References

*   **QAIC repository and IDL compiler:**
    <https://github.com/qualcomm/QAIC/>
*   **FastRPC documentation:**
    <https://github.com/qualcomm/fastrpc/blob/development/README.md>
