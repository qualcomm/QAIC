
# QAIC Testcases — Build & Run Guide (Arm64 & Cross‑Compile)

These samples demonstrate building and running testcases using the fully open‑source toolchain around the **QAIC IDL compiler**, **Clang/LLVM**, and the **LLVM linker**.

> **Repo:** QAIC (IDL compiler, examples) — https://github.com/qualcomm/QAIC/

---

## (1) Overview

The testcases are authored in QAIC IDL and C. A standard GNU Autotools flow is used for configuration and build, and the LLVM toolchain is used for compiling and linking.

- **Host build (aarch64/arm64):** Verified
- **x86_64 host cross‑compile to aarch64:** Supported (requires cross fastrpc libraries)

> If you plan to **cross‑compile on x86_64** to target aarch64 Linux, ensure the fastrpc cross libraries and runtime dependencies are installed.

---

## (2) Prerequisites

### Toolchain
- **LLVM/Clang** (version 14 or newer recommended; examples below use `ld.lld-20`)
- **GNU Autotools:** `autoconf`, `automake`, `libtool`

### Libraries (runtime/build)
- `libyaml`
- `libmd`
- `libbsd`

---

## (3) Cross‑compile notes (x86_64 → aarch64)

If you are building on **x86_64** and targeting **aarch64 (arm64)**, you will need:

- aarch64 cross toolchain (Clang/LLVM works well as a cross toolchain)
- fastrpc **cross build libraries** and runtime dependencies (`libyaml`, `libmd`, `libbsd`)

Follow the dependency installation instructions provided in the fastrpc project:

- **fastrpc README:** https://github.com/qualcomm/fastrpc/blob/development/README.md

Ensure your environment exposes the cross tools and libraries (e.g., `CC=clang`, `LD=ld.lld-20`, and appropriate `sysroot`/`-target aarch64-linux-gnu` flags if required by your setup).

---

## (4) Configure & Build

From the repository root:

```bash
# Generate Autotools files
$ autoreconf -fi

# Configure the project
$ ./configure

# Build with LLVM linker (example uses lld-20)
$ make LD=ld.lld-20
```

> If you are cross‑compiling, also ensure your environment exports the correct compiler/linker and search paths (e.g., `CC=clang`, `CFLAGS/CPPFLAGS/LDFLAGS` with your sysroot, and `--target=aarch64-linux-gnu` where appropriate). The exact flags depend on your toolchain layout.

Build artifacts for each testcase include the test binary and the generated skeleton/stub shared objects for the service.

---

## (5) Runtime layout & deployment

To run a testcase on the **target (aarch64) device**:

1. Copy the following files to the target (example for the `array` testcase):
   - `array_test` (test executable)
   - `libarray_skel.so` (service skeleton)
   - `libarray_stub.so` (client stub)
   - `libbsd.so.0`
   - `libmd.so` and `libmd.so.0`

2. Place them in a directory that the dynamic loader can search, e.g. `/usr/local/bin`:

```bash
$ scp array_test libarray_skel.so libarray_stub.so libbsd.so.0 libmd.so libmd.so.0 <user>@<target>:/usr/local/bin/
```

3. On the target, export the loader path and run the test:

```bash
$ export LD_LIBRARY_PATH=/usr/local/bin:$LD_LIBRARY_PATH
$ cd /usr/local/bin
$ ./array_test -d 3 -U 1
```

- `-d 3` – select domain (example uses CDSP=3; adjust per your platform)
- `-U 1` – example flag per testcase usage (consult each test’s `--help` or README, if present)

> Repeat the same pattern for other testcases (replace `array_*` with the corresponding testcase names and libraries).

---

## (6) Notes & tips

- **LLVM version:** The build command shown uses `LD=ld.lld-20`. If your system provides a different LLD version, change accordingly (e.g., `ld.lld`, `ld.lld-17`, etc.).
- **Clean builds:** If you switch targets or toolchains, prefer a clean build: `make distclean && autoreconf -fi && ./configure && make ...`.
- **Cross‑environment:** When cross‑compiling, validate that your `LD_LIBRARY_PATH` and rpath settings resolve to the correct architecture libraries on the target.

---

## (7) Troubleshooting

- **Linker not found:** Ensure `lld` (LLVM linker) is installed and available in `PATH`. Try `which ld.lld-20` or adjust to `LD=ld.lld`.
- **Missing libs at runtime:** If the test reports missing `libbsd.so.0` or `libmd.so`, verify they exist on the target in a directory included in `LD_LIBRARY_PATH`.
- **Cross‑compile errors:** Confirm you installed the cross fastrpc libraries and set the correct `--target`, sysroot, and include/lib search paths as documented in the fastrpc README.

---

## (8) License

This project uses the **BSD-3-Clause-Clear** license. Ensure all contributed source/IDL files include the appropriate SPDX header:

- C/C headers: `/* SPDX-License-Identifier: BSD-3-Clause-Clear */`
- IDL files: `// SPDX-License-Identifier: BSD-3-Clause-Clear`

---

## (9) References
- QAIC repository and IDL compiler: https://github.com/qualcomm/QAIC/
- fastrpc documentation (dependencies, cross libs): https://github.com/qualcomm/fastrpc/blob/development/README.md
