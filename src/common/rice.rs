use std::marker::PhantomData;
use std::mem::{forget, size_of};
use std::ops::{Deref, DerefMut};

const DEBUG: bool = true;

pub struct RiceInner<T: ?Sized> {
    pub count: usize,
    pub data: T,
}

impl<T> RiceInner<T> {
    pub fn increment(&mut self) {
        self.count += 1;
    }

    pub fn decrement(&mut self) {
        self.count -= 1;
    }
}

pub struct Rice<T> {
    pub inner: *mut RiceInner<T>,
    _marker: PhantomData<T>,
}

impl<T> Rice<T> {
    pub fn increment_strong_count(this: &mut Self) {
        unsafe { (*this.inner).increment() }
    }

    pub fn new(data: T) -> Self {
        let p = Box::leak(Box::new(RiceInner { count: 1, data }));
        Self {
            inner: p,
            _marker: Default::default(),
        }
    }

    pub unsafe fn intoRaw(self) -> *const T {
        let ptr = &(*self.inner).data as *const T;
        forget(self);

        ptr
    }

    pub unsafe fn fromRaw(ptr: *const T) -> Rice<T> {
        Self {
            inner: ptr.byte_offset(-((size_of::<usize>()) as isize)) as *mut RiceInner<T>,
            _marker: Default::default(),
        }
    }
}

impl<T> Deref for Rice<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.inner).data }
    }
}

impl<T> DerefMut for Rice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (*self.inner).data }
    }
}

impl<T> Drop for Rice<T> {
    fn drop(&mut self) {
        if DEBUG {
            unsafe { println!("rice is being dropped {}", (*(self.inner)).count) }
        }
        unsafe {
            (*self.inner).decrement();
            if (*self.inner).count == 0 {
                if DEBUG {
                    println!("deallocating rice");
                }
                drop(Box::from_raw(self.inner));
            }
        }
    }
}

impl<T> Clone for Rice<T> {
    fn clone(&self) -> Self {
        unsafe { (*self.inner).increment() }
        if DEBUG {
            println!("incrementing rice");
        }
        Self {
            inner: self.inner,
            _marker: Default::default(),
        }
    }
}