use std::marker::PhantomData;
use std::mem::{forget, size_of};
use std::ops::Deref;

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
    pub inner: *mut RiceInner<T>
}

impl<T> Rice<T> {
    pub fn new(data: T) -> Self {
        let p = Box::leak(Box::new(RiceInner { count: 1, data }));
        Self {
            inner: p
        }
    }

    pub unsafe fn intoRaw(self) -> *const T {
        let ptr = &(*self.inner).data as *const T;
        forget(self);

        ptr
    }

    pub unsafe fn fromRaw(ptr: *const T) -> Rice<T> {
        Self {
            inner: ptr.byte_offset((size_of::<usize>()) as isize * -1) as *mut RiceInner<T>
        }
    }
}

impl<T> Deref for Rice<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.inner).data }
    }
}

impl<T> Drop for Rice<T> {
    fn drop(&mut self) {
        unsafe {
            (*self.inner).decrement();
            if (*self.inner).count == 0 {
                drop(Box::from_raw(self.inner));
            }
        }
    }
}

impl<T> Clone for Rice<T> {
    fn clone(&self) -> Self {
        unsafe { (*self.inner).increment() }
        Self {
            inner: self.inner
        }
    }
}