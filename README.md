small interpreted stack based programing language

- build size around 500kb
- performance comparable or better than python
- extensible
- statically typed
- REPL + compiler
- no 3rd party dependencies
- syntax based on python/rust

example program to count lines of code

```rust
fn countFile(path: String): int {
if ! endsWith(path, ".vipl") {
return 0
}

print("file")
print(path)

file = readFile(path)
fileLen = strLen(file)
i = 0
counter = 1

while i < fileLen {
c = file[i]

if c == '\n' {
counter += 1
}

i += 1
}

return counter
}

fn countDir(path: String): int {
print("dir")
print(path)

files = ls(path)
i = 0
len = arrayLen(files)
counter = 0

while i < len {
buf = path + ("/" + (files[i]))
ft = fileType(buf)
if ft == 2 {
counter += countDir(buf)
}
else {
if ft == 1 {
counter += countFile(buf)
}
else {
print(ft)
print("ERROR")
}
}
i += 1
}
return counter
}

directory = "/home/vasabi/Downloads/vm-rust/src"
dirType = fileType(directory)
res = 0

if dirType == 0 {
print("invalid path")
assert(1, 0)
}
else {
if dirType == 1 {
res += countFile(directory)
}
else {
res += countDir(directory)
}
}
print(res)
```