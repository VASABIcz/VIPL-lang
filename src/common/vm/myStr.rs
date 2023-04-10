use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Eq)]
pub enum MyStr {
    Static(&'static str),
    Runtime(Box<str>),
}

impl PartialEq for MyStr {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Hash for MyStr {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl From<Box<str>> for MyStr {
    #[inline]
    fn from(value: Box<str>) -> Self {
        Self::Runtime(value)
    }
}

impl From<String> for MyStr {
    #[inline]
    fn from(value: String) -> Self {
        Self::Runtime(value.into_boxed_str())
    }
}

impl From<&'static str> for MyStr {
    #[inline]
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

impl Display for MyStr {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MyStr::Static(v) => {
                write!(f, "{v}")
            }
            MyStr::Runtime(v) => {
                write!(f, "{v}")
            }
        }
    }
}

impl MyStr {
    #[inline]
    pub fn as_str(&self) -> &str {
        match self {
            MyStr::Static(s) => s,
            MyStr::Runtime(v) => v,
        }
    }
}