use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::error::Error;

use crate::errors::{Errorable, LoadFileError, ParserError};
use crate::fastAccess::FastAcess;
use crate::lexer::{
    tokenize, AlphabeticKeywordLexingUnit, IdentifierLexingUnit, KeywordLexingUnit, LexingUnit,
    RangeLexingUnit, SourceProvider, WhitespaceLexingUnit,
};

use crate::parser::ParsingUnitSearchType::Ahead;
use crate::parser::{parseOne, ParsingUnit, ParsingUnitSearchType, TokenProvider};
use crate::std::json::JsonToken::{
    ArrayBegin, ArrayEnd, Comma, False, Identifier, Null, ObjectBegin, ObjectEnd, Sep, True,
};
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::Bool;
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::namespace::{Namespace, StructMeta};
use crate::vm::nativeObjects::{ObjectType, ViplNativeObject, ViplObject, ViplObjectMeta};
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

#[derive(Debug, Clone)]
pub enum JSON {
    JObject(HashMap<String, JSON>),
    JArray(Vec<JSON>),
    JBool(bool),
    JChar(char),
    JInt(isize),
    JFloat(f64),
    JString(String),
    JNull,
}

impl Allocation for JSON {
    fn collectAllocations(&self, allocations: &mut HayCollector) {}
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum JsonToken {
    ObjectBegin,
    ObjectEnd,
    ArrayBegin,
    ArrayEnd,
    Sep,
    Comma,

    Identifier,
    Number,

    Null,
    True,
    False,
}

#[derive(Debug)]
struct JObjectParsingUnit;

impl ParsingUnit<JSON, JsonToken> for JObjectParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }
    fn canParse(&self, tokenProvider: &TokenProvider<JsonToken>, previous: Option<&JSON>) -> bool {
        tokenProvider.isPeekType(ObjectBegin)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<JsonToken>,
        previous: Option<JSON>,
        parser: &[Box<dyn ParsingUnit<JSON, JsonToken>>],
    ) -> Result<JSON, ParserError<JsonToken>> {
        let mut pairs = HashMap::new();

        tokenProvider.getAssert(ObjectBegin)?;

        let parsed = tokenProvider.parseManyWithSeparatorUntil(
            |it| {
                let key = it.getAssert(Identifier)?.str.clone();
                it.getAssert(Comma)?;
                let value = parseOne(it, Ahead, parser, None)?;

                Ok((key, value))
            },
            Some(Sep),
            ObjectEnd,
        )?;

        for p in parsed {
            pairs.insert(p.0, p.1);
        }

        Ok(JSON::JObject(pairs))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct JArrayParsingUnit;

impl ParsingUnit<JSON, JsonToken> for JArrayParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<JsonToken>, previous: Option<&JSON>) -> bool {
        tokenProvider.isPeekType(ArrayBegin)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<JsonToken>,
        previous: Option<JSON>,
        parser: &[Box<dyn ParsingUnit<JSON, JsonToken>>],
    ) -> Result<JSON, ParserError<JsonToken>> {
        tokenProvider.getAssert(ArrayBegin);

        let res = tokenProvider.parseManyWithSeparatorUntil(
            |it| parseOne(it, Ahead, parser, None),
            Some(Sep),
            ArrayEnd,
        )?;

        Ok(JSON::JArray(res))
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

#[derive(Debug)]
struct JKeywordParsingUnit;

impl ParsingUnit<JSON, JsonToken> for JKeywordParsingUnit {
    fn getType(&self) -> ParsingUnitSearchType {
        Ahead
    }

    fn canParse(&self, tokenProvider: &TokenProvider<JsonToken>, previous: Option<&JSON>) -> bool {
        tokenProvider.isPeekType(True)
            || tokenProvider.isPeekType(False)
            || tokenProvider.isPeekType(True)
            || tokenProvider.isPeekType(Null)
            || tokenProvider.isPeekType(Identifier)
    }

    fn parse(
        &self,
        tokenProvider: &mut TokenProvider<JsonToken>,
        previous: Option<JSON>,
        parser: &[Box<dyn ParsingUnit<JSON, JsonToken>>],
    ) -> Result<JSON, ParserError<JsonToken>> {
        if tokenProvider.isPeekType(True) {
            tokenProvider.getAssert(True)?;

            Ok(JSON::JBool(true))
        } else if tokenProvider.isPeekType(False) {
            tokenProvider.getAssert(False)?;
            Ok(JSON::JBool(false))
        } else if tokenProvider.isPeekType(Null) {
            tokenProvider.getAssert(Null)?;
            Ok(JSON::JNull)
        } else if tokenProvider.isPeekType(Identifier) {
            let t = tokenProvider.getAssert(Identifier)?;

            Ok(JSON::JString(t.str.clone()))
        } else {
            unreachable!()
        }
    }

    fn getPriority(&self) -> usize {
        todo!()
    }

    fn setPriority(&mut self, priority: usize) {
        todo!()
    }
}

pub fn jsonParsingUnits() -> Vec<Box<dyn ParsingUnit<JSON, JsonToken>>> {
    vec![
        Box::new(JObjectParsingUnit),
        Box::new(JArrayParsingUnit),
        Box::new(JKeywordParsingUnit),
    ]
}

pub fn jsonTokenizingUnits() -> Vec<Box<dyn LexingUnit<JsonToken>>> {
    vec![
        AlphabeticKeywordLexingUnit::new("false", JsonToken::False),
        AlphabeticKeywordLexingUnit::new("true", JsonToken::False),
        AlphabeticKeywordLexingUnit::new("null", JsonToken::False),
        KeywordLexingUnit::new("{", JsonToken::ObjectBegin),
        KeywordLexingUnit::new("}", JsonToken::ObjectEnd),
        KeywordLexingUnit::new("[", JsonToken::ArrayBegin),
        KeywordLexingUnit::new("]", JsonToken::ArrayEnd),
        KeywordLexingUnit::new(",", JsonToken::Sep),
        KeywordLexingUnit::new(":", JsonToken::Comma),
        RangeLexingUnit::new("\"", "\"", Some(JsonToken::Identifier)),
        WhitespaceLexingUnit::new(),
        IdentifierLexingUnit::new(JsonToken::Identifier), // we do lil bit of troling :D
    ]
}

impl JSON {
    pub fn parse(s: &str) -> Result<JSON, LoadFileError<JsonToken>> {
        let res = tokenize(
            &mut jsonTokenizingUnits(),
            SourceProvider {
                data: s,
                index: 0,
                row: 0,
                col: 0,
            },
        )?;
        let units = jsonParsingUnits();

        let mut provider = TokenProvider::new(res);

        let res = parseOne(&mut provider, Ahead, &units, None)?;

        Ok(res)
    }
}

pub fn registerJson(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("js", vm);

    n.registerStruct(StructMeta::n(
        "JBool",
        FastAcess::ofStr(vec![("v", VariableMetadata::n("v", Bool))]),
    ));

    n.makeNative(
        "load",
        &[DataType::str()],
        |vm, s| {
            let str = s.getString(0);

            let v = match JSON::parse(str).ok() {
                None => Value::null(),
                Some(v) => {
                    let obj = ViplObject {
                        meta: ViplObjectMeta {
                            namespaceId: 0,
                            structId: 0,
                            objectType: ObjectType::Native(ViplNativeObject::default()),
                        },
                        data: v,
                    };
                    vm.allocate(obj).into()
                }
            };

            v
        },
        DataType::obj("Json"),
        false,
    );

    n.makeNative(
        "save",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::str(),
        false,
    );

    n.makeNative(
        "getIndex",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("Json"),
        false,
    );

    n.makeNative(
        "length",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("JInt"),
        false,
    );

    n.makeNative(
        "hasKey",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("JBool"),
        false,
    );

    n.makeNative(
        "getField",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("Json"),
        false,
    );

    n.makeNative(
        "asString",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::str(),
        false,
    );

    n.makeNative(
        "asInt",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("JInt"),
        false,
    );

    n.makeNative(
        "asBool",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("JBool"),
        false,
    );

    n.makeNative(
        "asNull",
        &[DataType::obj("Json")],
        |vm, s| todo!(),
        DataType::obj("JNull"),
        false,
    );

    vm.registerNamespace(n);
}

#[test]
fn testParse() {
    let parsed = JSON::parse("{hello: world, kawaii: [OwO, UwU]}").unwrap();

    println!("{:?}", parsed)
}
