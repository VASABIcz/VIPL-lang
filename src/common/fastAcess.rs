use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug)]
pub struct FastAcess<LOOKUP: Debug, VALUE: Debug> {
    pub lookupTable: HashMap<LOOKUP, usize>,
    pub actual: Vec<VALUE>
}

impl<LOOKUP: Debug, VALUE: Debug> Default for FastAcess<LOOKUP, VALUE> {
    fn default() -> Self {
        Self{ lookupTable: HashMap::default(), actual: vec![] }
    }
}

impl<VALUE: Debug> FastAcess<String, VALUE> {
    pub fn getSlowStr(&self, key: &str) -> Option<(&VALUE, usize)> {
        let v = *self.lookupTable.get(key)?;
        let s = self.actual.get(v)?;

        Some((s, v))
    }
}

impl<LOOKUP: Hash + Eq + PartialEq + Debug, VALUE: Debug> FastAcess<LOOKUP, VALUE> {
    #[inline(always)]
    pub fn getFast(&self, key: usize) -> Option<&VALUE> {
        self.actual.get(key)
    }

    #[inline(always)]
    pub fn getFastMut(&mut self, key: usize) -> Option<&mut VALUE> {
        self.actual.get_mut(key)
    }

    pub fn getSlow(&self, key: &LOOKUP) -> Option<(&VALUE, usize)> {
        let v = *self.lookupTable.get(key)?;
        let s = self.actual.get(v)?;

        Some((s, v))
    }

    pub fn insert(&mut self, key: LOOKUP, value: VALUE) -> Option<usize> {
        self.actual.push(value);
        let index = self.actual.len()-1;

        self.lookupTable.insert(key, index);

        Some(index)
    }

    pub fn replaceValue(&mut self, key: usize, value: VALUE) {
        *self.actual.get_mut(key).unwrap() = value;
    }
}