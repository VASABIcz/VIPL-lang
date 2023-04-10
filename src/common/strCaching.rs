use std::collections::HashMap;
use std::sync::Mutex;
use crate::vm::objects::ViplObject;


static strCache: Option<Mutex<HashMap<String, ViplObject>>> = None;