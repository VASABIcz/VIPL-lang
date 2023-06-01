#!/bin/python3

import os

bRes = os.system("cargo build --package vipl --release --bin compiler")

if bRes != 0:
    print("build failed")
    exit(-1)

bRes = os.system("cargo build --package vipl --bin compiler")

if bRes != 0:
    print("build failed")
    exit(-1)

EXECUTABLES = {
    "release": "target/release/compiler",
    "debug": "target/debug/compiler"
}
TEST_FOLDERS = ["examples"]


for folder in TEST_FOLDERS:
    for file in os.listdir(folder):
        if not file.endswith(".vipl"):
            continue

        for name, executable in EXECUTABLES.items():
            res = os.system(f"RUST_BACKTRACE=1 ./{executable} {folder}/{file}")

            if res != 0:
                print(f"[ERR] {name} ./{executable} {folder}/{file} exited with {res}")
                exit(-1)
            else:
                print(f"[OK] {name} ./{executable} {folder}/{file} {res}")