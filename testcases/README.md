
# QAIC Testcases

## Overview

The testcases are authored in QAIC IDL and C. A standard GNU Autotools flow is used for configuration and build, and the LLVM toolchain is used for compiling and linking.

- **Host build (aarch64/arm64):** Supported
- **x86_64 host cross-compile to aarch64:** Supported (requires cross fastrpc libraries)

---

## Prerequisites

### Toolchain
- **LLVM/Clang** (version 14 or newer recommended; linker examples use `ld.lld-20`)
- **GNU Autotools:** `autoconf`, `automake`, `libtool`

### Libraries (runtime/build)
- `libyaml`
- `libmd`
- `libbsd`
- `libcdsprpc` (from fastrpc)

---

## Cross-compile notes (x86_64 → aarch64)

On **x86_64** and targeting **aarch64 (arm64)**, you will need:

- an aarch64 cross toolchain (Clang/LLVM works well as a cross toolchain)
- fastrpc **cross build libraries** and runtime dependencies (`libyaml`, `libmd`, `libbsd`)

Follow the dependency installation instructions provided in the fastrpc project:

- **fastrpc README:** https://github.com/qualcomm/fastrpc/blob/development/README.md#steps-to-cross-compile-the-project-on-ubuntu

Ensure your environment exposes the cross tools and libraries (for example: `CC=clang`, `LD=ld.lld-20`, and appropriate `sysroot` / `-target aarch64-linux-gnu` flags if required by your setup).

---

## Configure & Build

From the repository root:

```bash
# Generate Autotools files
autoreconf -fi

# Clean previous configure/build outputs (recommended)
make distclean

# FastRPC dependency (required)

These testcases require the FastRPC userspace library `libcdsprpc`:

- **Build-time:** `libcdsprpc` (for your target arch) and FastRPC headers
- **Run-time (target):** `libcdsprpc.so` must be available on the target

# Configure (pass TEST_LDFLAGS so the link can find libcdsprpc)

Built FastRPC from source using libtool, the shared library is generated under:

`<FASTRPC_SRC>/src/.libs/`  (e.g. contains `libcdsprpc.so`)
Configure the testcases by pointing to that directory:

```bash
FASTRPC_SRC=<path-to-fastrpc-shared-library>
TEST_LDFLAGS="-L$FASTRPC_SRC/src/.libs -lcdsprpc" ./configure

# Build
make

> During cross-compiling, also ensure your environment exports the correct compiler/linker and search paths (e.g., `CC=clang`, `CFLAGS/LDFLAGS` with your sysroot, and `--target=aarch64-linux-gnu` where appropriate). The exact flags depend on your toolchain layout.

Build artifacts for each testcase include the test binary and the generated skeleton/stub shared objects for the service.

---

## Runtime layout & deployment

To run a testcase on the **target (aarch64) device**:

Copy the following files to the target (example for the `array` testcase):
- `array_test` (test executable)
- `libarray_skel.so` (service skeleton)
- `libarray_stub.so` (client stub)
- `libbsd.so.0`
- `libmd.so` and `libmd.so.0`

Place them in a directory that the dynamic loader can search, e.g. `/usr/local/bin`:

# Deploy binaries to target
Push the test binary and required shared libraries to the target using `adb`:
```

On the target, export the loader path and run the test:

```bash
export LD_LIBRARY_PATH=/usr/local/bin:$LD_LIBRARY_PATH
cd /usr/local/bin
./array_test -d 3 -U 1

## Notes & tips

- **Clean builds:** If you want to clean and rebuild the binaries, prefer a clean build: "make distclean".
- **Cross‑environment:** When cross‑compiling, validate that your `LD_LIBRARY_PATH` resolve to the correct architecture libraries on the target.
- **PD support:** These testcases are not supported on signed PD.
- **Validation status:** Execution has been validated only on unsigned PD.

---

## Troubleshooting

- **Linker not found:** Ensure `lld` (LLVM linker) is installed and available in `PATH`. Try `which ld.lld-20` or adjust to `LD=ld.lld`.
- **Missing libs at runtime:** If the test reports missing `libbsd.so.0` or `libmd.so`, `libmd.so.0` verify they exist on the target in a directory included in `LD_LIBRARY_PATH`.
- **Cross‑compile errors:** Confirm you installed the cross fastrpc libraries and set the correct `--target`, sysroot, and include/lib search paths as documented in the fastrpc README.

---

## License

This project uses the **BSD-3-Clause-Clear** license. Ensure all contributed source/IDL files include the appropriate SPDX header:

- C/C headers: `/* SPDX-License-Identifier: BSD-3-Clause-Clear */`
- IDL files: `// SPDX-License-Identifier: BSD-3-Clause-Clear`

---

## References
- QAIC repository and IDL compiler: https://github.com/qualcomm/QAIC/
- fastrpc documentation (dependencies, cross libs): https://github.com/qualcomm/fastrpc/blob/development/README.md
