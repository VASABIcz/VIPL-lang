#!/bin/python3

import os

EXECUTABLES = {
    "release": "target/release/namespaced",
    "debug": "target/debug/namespaced"
}
TEST_FOLDERS = ["examples"]


for folder in TEST_FOLDERS:
    for file in os.listdir(folder):
        if not file.endswith(".vipl"):
            continue

        for name, executable in EXECUTABLES.items():
            res = os.system(f"./{executable} {folder}/{file}")

            if res != 0:
                print(f"[ERR] {name} ./{executable} {folder}/{file} {res}")
            else:
                print(f"[OK] {name} ./{executable} {folder}/{file} {res}")