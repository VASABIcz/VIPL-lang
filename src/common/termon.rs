use libc::{c_int, ICANON, termios};

pub struct Termon {
    inputBuf: String,
    index: usize
}

impl Termon {
    pub fn clear(&mut self) {
        goLeftBy(self.index);
        clearFromTo(0, self.inputBuf.len());
        goLeftBy(self.inputBuf.len());

        self.index = 0;
        self.inputBuf.clear();
    }

    pub fn moveRight(&mut self) {
        if self.index == self.inputBuf.len() {
            return;
        }

        self.index += 1;
    }

    pub fn moveLeft(&mut self) {
        if self.index == 0 {
            return;
        }

        self.index -= 1;
    }

    pub fn insert(&mut self, c: char) {
        if self.index == self.inputBuf.len() {
            self.inputBuf.push(c);
            self.index += 1;
        }
        else {
            let mut save = self.inputBuf.clone();
            save.insert(self.index, c);

            let save1 = self.index;

            self.clear();

            putStr(&save);
            self.inputBuf = save;

            self.index = save1+1;
            goLeftBy(self.inputBuf.len()-self.index);
        }
    }

    pub fn write(&mut self, s: &str) {
        for c in s.chars() {
            self.insert(c);
        }
    }

    pub fn delete(&mut self) {
        todo!()
    }

    pub fn getBuf(&self) -> &str {
        &self.inputBuf
    }
}

pub fn enableRawMode() {
    let mut t = termios{
        c_iflag: 0,
        c_oflag: 0,
        c_cflag: 0,
        c_lflag: 0,
        c_line: 0,
        c_cc: [0; 32],
        c_ispeed: 0,
        c_ospeed: 0,
    };


    unsafe { libc::tcgetattr(0, &mut t) };

    // disable ECHO and ICANON mode
    t.c_lflag &= !(libc::ECHO|ICANON);

    unsafe { libc::tcsetattr(0, libc::TCSANOW, &mut t); }
}

pub fn getChar() -> char {
    unsafe {
        libc::getchar() as u8 as char
    }
}

pub fn putChar(c: char) {
    unsafe {
        libc::putchar(c as c_int);
    }
}

pub fn clearScreen() {
    putEscapeStr("H");
    putEscapeStr("J");
}

pub fn putEscape(code: u8) {
    putChar(0x1B as char);
    putChar(91 as char);
    putChar(code as char);
}

pub fn putEscapeStr(code: &str) {
    putChar(0x1B as char);
    putChar(91 as char);

    for b in code.bytes() {
        putChar(b as char);
    }
}

pub fn getLocation() -> (usize, usize) {
    putEscapeStr("6n");

    let mut buf = vec![];

    while let char = getChar() {
        if char == 'R' {
            break
        }
        buf.push(char as u8);
    }
    buf.remove(0);
    buf.remove(0);
    let str = String::from_utf8_lossy(&buf);

    let v = str.split(';').collect::<Vec<_>>();

    let x = v[0].parse::<usize>().unwrap();
    let y = v[1].parse::<usize>().unwrap();

    (x, y)
}

fn clearFromTo(start: usize, end: usize) {
    for _ in start..end {
        putChar(' ');
    }
}

fn goRightBy(n: usize) {
    for _ in 0..n {
        putEscape('C' as u8);
    }
}

fn goLeftBy(n: usize) {
    for _ in 0..n {
        putChar(8 as char);
    }
}

pub fn putStr(s: &str) {
    for b in s.bytes() {
        putChar(b as char);
    }
}

pub fn readRaw(prev: &[String]) -> String {
    let mut currentBuf = String::new();

    let mut buf = String::new();
    let mut index = 0usize;
    let mut historyIndex: Option<usize> = None;

    loop {
        let c = getChar();

        // enter
        if c == '\n' {
            putChar('\n');
            return buf;
            continue
        }
        // back space
        if c == 127 as char {
            if buf.is_empty() {
                continue
            }
            if index == buf.len() {
                buf.pop();
                putChar(8 as char);
                putChar(' ');
                putChar(8 as char);

                index -= 1;
            }
            else if index != 0 {
                let oldIndex = index;

                goLeftBy(index);
                clearFromTo(0, buf.len());
                goLeftBy(buf.len());

                buf.remove(index-1);
                putStr(&buf);

                index = buf.len();

                goLeftBy(index-oldIndex+1);
                index -= index-oldIndex+1;
            }
            continue
        }
        // escape sequence
        if c == 0x1B as char {
            getChar();
            let code = getChar();

            match code as u8 {
                // up
                65 => {
                    if prev.is_empty() || historyIndex.is_some_and(|it| it == 0) {
                        continue
                    }

                    goLeftBy(index);
                    clearFromTo(0, buf.len());
                    goLeftBy(buf.len());

                    if historyIndex == None {
                        historyIndex = Some(prev.len()-1);
                        currentBuf = buf.clone();

                        let hIndex = historyIndex.unwrap();

                        buf = prev[hIndex].clone();

                        putStr(&buf);
                        index = buf.len();
                    }
                    else {
                        let hIndex = historyIndex.unwrap();
                        historyIndex = Some(hIndex-1);


                        buf = prev[hIndex-1].clone();

                        putStr(&buf);
                        index = buf.len();
                    }
                }
                // down
                66 => {
                    if historyIndex == None {
                        continue
                    }

                    goLeftBy(index);
                    clearFromTo(0, buf.len());
                    goLeftBy(buf.len());

                    if historyIndex.is_some_and(|it| it == prev.len()-1) {
                        historyIndex = None;

                        buf = currentBuf.clone();
                        putStr(&buf);
                        index = buf.len();

                        continue
                    }

                    let hIndex = historyIndex.unwrap();
                    historyIndex = Some(hIndex+1);


                    buf = prev[hIndex+1].clone();

                    putStr(&buf);
                    index = buf.len();

                }
                // right
                67 => {
                    if index == buf.len() {
                        continue
                    }

                    putEscape('C' as u8);
                    index += 1;
                }
                // left
                68 => {
                    if index == 0 {
                        continue
                    }

                    putChar(8 as char);
                    index -= 1;
                }
                _ => {}
            }
            continue
        }

        if index == buf.len() {
            buf.push(c);
            putChar(c);
            index += 1;
        }
        else {
            let oldIndex = index;

            goLeftBy(index);
            clearFromTo(0, buf.len());
            goLeftBy(buf.len());

            buf.insert(index, c);
            putStr(&buf);

            index = buf.len();

            goLeftBy(index-(oldIndex+1));
            index -= (index-(oldIndex+1));
        }
    }
}