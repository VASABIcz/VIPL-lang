#!/bin/python3

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


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
                print(f"{bcolors.FAIL}[ERR] {name} ./{executable} {folder}/{file} exited with {res} {bcolors.ENDC}")
                exit(-1)
            else:
                print(f"{bcolors.OKGREEN}[OK] {name} ./{executable} {folder}/{file} {res} {bcolors.ENDC}")