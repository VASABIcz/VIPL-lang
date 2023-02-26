use std::collections::HashMap;
use std::sync::Mutex;
use crate::ast::Op;
use crate::objects::ViplObject;

static strCache: Option<Mutex<HashMap<String, ViplObject>>> = None;