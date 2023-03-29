use std::collections::HashMap;
use std::sync::Mutex;

use crate::objects::ViplObject;

static strCache: Option<Mutex<HashMap<String, ViplObject>>> = None;