use std::collections::HashSet;
use libc::isdigit;
use crate::errors::Errorable;
use crate::lexer::SourceProvider;
use crate::regix::Regix::{Char, Not};

#[derive(Debug)]
pub enum Regix {
    Any,
    Numeric,
    Whitespace,
    Letter,
    Char(char),
    XAndMore {
        inner: Box<Regix>,
        amount: usize
    },
    Optional(Box<Regix>),
    Capture {
        inner: Vec<Regix>,
        id: usize
    },
    Group(Vec<Regix>),
    Or {
        right: Box<Regix>,
        left: Box<Regix>
    },
    Not(Box<Regix>)
}

impl Regix {
    fn parseSimpleRaw(l: &mut SourceProvider, regixes: &mut Vec<Regix>, captures: &mut usize) -> Errorable<()> {
        let mut buf = vec![];

        while l.isPeekChar(|c| {
            let invalidChars: HashSet<char> = HashSet::from_iter(vec!['(', '[', '|', '?', '*', '+', '.', '^', ']', ')']);
            !invalidChars.contains(&c)
        }) {
            let c = l.assertChar()?;
            if c == '\\' {
                let n = l.assertChar()?;

                let reg = match n {
                    'l' => Regix::Letter,
                    'd' => Regix::Numeric,
                    'w' => Regix::Whitespace,
                    o => Regix::Char(o)
                };
                buf.push(reg)
            }
            else {
                buf.push(Regix::Char(c))
            }
        }
        if buf.is_empty() {
            None.ok_or("failed to parse simple regix")?;
        }
        regixes.push(Regix::Group(buf));

        Ok(())
    }

    fn parseRaw(l: &mut SourceProvider, regixes: &mut Vec<Regix>, captures: &mut usize) -> Errorable<()> {
        if l.isPeek("(") {
            l.consumeOne();

            let mut buf = vec![];

            while !l.isPeek(")") {
                Self::parseRaw(l, &mut buf, captures);
            }
            l.assertConsume(")")?;

            regixes.push(Regix::Capture{ inner: buf, id: *captures });
            *captures += 1;
        }
        else if l.isPeek("[") {
            l.consumeOne();

            let mut buf = vec![];

            while !l.isPeek("]") {
                Self::parseRaw(l, &mut buf, captures);
            }
            l.assertConsume("]")?;

            regixes.push(Regix::Group(buf));
            *captures += 1;
        }
        else if l.isPeek("|") {
            l.consumeOne();
            let prev = regixes.pop().ok_or("expected previous regix for or")?;
            let mut buf = vec![];
            Self::parseRaw(l, &mut buf, captures);
            if buf.len() != 1 {
                None.ok_or("or missing right side")?;
            }
            regixes.push(Regix::Or { right: Box::new(buf.pop().unwrap()), left: Box::new(prev) })
        }
        else if l.isPeek("?") {
            l.consumeOne();

            let prev = regixes.pop().ok_or("expected previous regix for optional")?;

            regixes.push(Regix::Optional(Box::new(prev)))
        }
        else if l.isPeek("*") {
            l.consumeOne();

            let prev = regixes.pop().ok_or("expected previous regix for zero and more")?;

            regixes.push(Regix::XAndMore{ inner: Box::new(prev), amount: 0 })
        }
        else if l.isPeek("+") {
            l.consumeOne();

            let prev = regixes.pop().ok_or("expected previous regix for one and more")?;

            regixes.push(Regix::XAndMore{ inner: Box::new(prev), amount: 1 })
        }
        else if l.isPeek(".") {
            l.consumeOne();

            regixes.push(Regix::Any)
        }
        else if l.isPeek("^") {
            l.consumeOne();

            let mut buf = vec![];
            Self::parseRaw(l, &mut buf, captures);
            if buf.len() != 1 {
                None.ok_or("not missing right side")?;
            }

            regixes.push(Not(Box::new(buf.pop().unwrap())))
        }
        else {
            Self::parseSimpleRaw(l, regixes, captures);
        }
        Ok(())
    }

    pub fn parse(str: &str) -> Regix {
        let mut regixes = vec![];
        let mut groups = 0usize;
        let mut l = SourceProvider{ data: str, index: 0 };

        while !l.isDone() {
            Regix::parseRaw(&mut l, &mut regixes, &mut groups);
        }

        Regix::Group(regixes)
    }

    pub fn matchStr<'a>(&self, s: &'a str, matches: &mut Vec<Vec<&'a str>>) -> Option<usize> {
        match self {
            Regix::Any => {
                if s.is_empty() {
                    None
                }
                else {
                    Some(1)
                }
            }
            Regix::Numeric => {
                if s.is_empty() { return None }

                if s.chars().next().unwrap().is_digit(10) {
                    Some(1)
                }
                else {
                    None
                }
            }
            Regix::Whitespace => {
                if s.is_empty() { return None }

                if s.chars().next().unwrap().is_whitespace() {
                    Some(1)
                }
                else {
                    None
                }
            }
            Regix::Letter => {
                if s.is_empty() { return None }

                if s.chars().next().unwrap().is_alphabetic() {
                    Some(1)
                }
                else {
                    None
                }
            }
            Char(c) => {
                if s.is_empty() { return None }

                if s.chars().next().unwrap() == *c {
                    Some(1)
                }
                else {
                    None
                }
            }
            Regix::XAndMore { inner, amount } => {
                let mut matchCount = 0usize;
                let mut matchAmount = 0usize;

                let mut buf = s;

                loop {
                    match inner.matchStr(buf, matches) {
                        None => {
                            if matchCount >= *amount {
                                return Some(matchAmount)
                            }
                            else {
                                return None
                            }
                        }
                        Some(v) => {
                            matchCount += 1;
                            matchAmount += v;

                            buf = &s[matchAmount..]
                        }
                    }
                }
            }
            Regix::Optional(inner) => {
                match inner.matchStr(s, matches) {
                    None => Some(0),
                    Some(v) => Some(v)
                }
            }
            Regix::Capture { inner, id } => {
                let mut matchAmount = 0usize;

                for i in inner {
                    match i.matchStr(&s[matchAmount..], matches) {
                        None => {
                            return None
                        }
                        Some(v) => {
                            matchAmount += v
                        }
                    }
                }

                while matches.len() <= *id {
                    matches.push(vec![])
                }

                matches[*id].push(&s[..matchAmount]);

                Some(matchAmount)
            }
            Regix::Group(inner) => {
                let mut matchAmount = 0usize;

                for i in inner {
                    match i.matchStr(&s[matchAmount..], matches) {
                        None => {
                            return None
                        }
                        Some(v) => {
                            matchAmount += v
                        }
                    }
                }
                Some(matchAmount)
            }
            Regix::Or { right, left } => {
                match left.matchStr(s, matches) {
                    Some(v) => Some(v),
                    None => right.matchStr(s, matches)
                }
            }
            Not(inner) => {
                match inner.matchStr(s, matches) {
                    None => Some(1),
                    Some(_) => None
                }
            }
        }
    }
}

#[test]
fn te() {
    let simpleRegix = Regix::parse("(UwU|OwO+)");
    let mut buf = vec![];
    let res = simpleRegix.matchStr("UwUOwO", &mut buf);
    println!("{:?} {:?}", res, buf)
}