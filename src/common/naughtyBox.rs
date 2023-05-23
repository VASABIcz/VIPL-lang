#[derive(Clone, Copy, Debug)]
pub struct Naughty<T> {
    pub inner: *mut T
}

impl<T> Naughty<T> {
    pub fn getMut(&mut self) -> &mut T {
        unsafe { &mut *self.inner }
    }

    pub fn new(v: *mut T) -> Naughty<T> {
        Self {
            inner: v,
        }
    }
}