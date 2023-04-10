use std::collections::HashSet;

use std::ops::{Deref, DerefMut};
use std::process::exit;

const DEBUG: bool = false;

pub trait Allocation {
    fn collectAllocations(&self, allocations: &mut HayCollector);
}

#[derive(Default)]
pub struct HayCollector {
    pub visited: HashSet<usize>
}

impl HayCollector {
    #[inline]
    pub fn new() -> Self {
        Self {
            visited: Default::default(),
        }
    }

    #[inline]
    pub fn visit(&mut self, ptr: usize) {
        self.visited.insert(ptr);
    }

    #[inline]
    pub fn visitHay<T: Allocation>(&mut self, hay: Hay<T>) {
        self.visited.insert(hay.inner as usize);
    }
}

#[derive(Debug)]
pub struct Hay<T: Allocation> {
    pub inner: *mut T
}

impl<T: Allocation> Clone for Hay<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self{ inner: self.inner }
    }
}

impl<T: Allocation> Copy for Hay<T> {}

impl<T: Allocation> Hay<T> {
    #[inline]
    pub fn new(ptr: *mut T) -> Hay<T> {
        Self {
            inner: ptr,
        }
    }
}

impl<T: Allocation> Deref for Hay<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.inner }
    }
}

impl<T: Allocation> DerefMut for Hay<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.inner }
    }
}

#[derive(Debug, Default)]
pub struct Heap {
    pub allocations: HashSet<usize>
}

impl Heap {
    #[inline]
    pub fn allocate<T: Allocation>(&mut self, value: T) -> Hay<T> {
        let b = Box::new(value);
        let ptr = Box::into_raw(b);

        if DEBUG {
            println!("allocated {}", ptr as usize);
        }

        self.allocations.insert(ptr as usize);

        Hay::new(ptr)
    }

    pub fn gc(&mut self, collected: HayCollector) {
        let unreachable = self.allocations.difference(&collected.visited);
        let mut count = 0;
        for u in unreachable {
            count += 1;
            unsafe { Box::from_raw(*u as *mut ()) };
        }
        if DEBUG {
            println!("freed {} object", count);
            println!("currently allocated {}", collected.visited.len());
        }
        self.allocations = collected.visited
    }

    pub fn contains(&self, ptr: usize) -> bool {
        self.allocations.contains(&ptr)
    }
}