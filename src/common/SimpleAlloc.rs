use std::alloc::{alloc, Layout};
use std::ops::DerefMut;
use std::ptr;

struct AllocBlock<const SIZE: usize> {
    next: Option<Box<AllocBlock<SIZE>>>,
    data: [u8; SIZE]
}

impl<const SIZE: usize> AllocBlock<SIZE> {
    pub fn new() -> AllocBlock<SIZE> {
        Self {
            next: None,
            data: [0; SIZE],
        }
    }

    pub fn nextMut(&mut self) -> (&mut AllocBlock<SIZE>, bool) {
        let mut allocated = false;

        if self.next.is_none() {
            self.next = Some(Box::new(AllocBlock::new()));
            allocated = true;
        }

        match &mut self.next {
            None => panic!(),
            Some(v) => (v, allocated)
        }
    }
}

struct FreeSpot {
    size: usize,
    index: usize
}

impl FreeSpot {
    pub fn new(index: usize, size: usize) -> Self {
        Self {
            size,
            index,
        }
    }
}

struct SimpleAlloc<const SIZE: usize> {
    block: Box<AllocBlock<SIZE>>,
    unallocated: Vec<FreeSpot>
}

impl<const SIZE: usize> SimpleAlloc<SIZE> {
    pub fn new() -> SimpleAlloc<SIZE> {
        SimpleAlloc{
            block: Box::new(AllocBlock::new()),
            unallocated: vec![FreeSpot::new(0, SIZE)]
        }
    }

    pub fn allocate(&mut self, size: usize) -> Option<*mut u8> {
        assert!(size <= SIZE);

        // TODO cache the biggest available allocation + actualy allocate new block
        let (index, b) = self.findFreeBlock(size)?;

        if b.size == size {
            self.unallocated.remove(index);
        }
        else {
            let bSize = self.unallocated[index].size;
            let bIndex = self.unallocated[index].index;

            let bMut = self.unallocated.get_mut(index).unwrap();

            bMut.size = bSize - size;
            bMut.index = bIndex + size;
        }

        let mut node = &mut *self.block;

        for o in 0..(index/SIZE) {
            let (n, didAllocate) = node.nextMut();

            // FIXME this should never be called
            if didAllocate {
                // NOTE: we have to prevent connecting blocks between allocations

                self.unallocated.push(FreeSpot::new(o*SIZE, SIZE))
            }

            node = n
        }

        Some(node.data.get_mut(index % SIZE).unwrap() as *mut u8)
    }

    pub fn free(&mut self, ptr: *mut u8, size: usize) {
        todo!()
    }

    fn findFreeBlock(&self, size: usize) -> Option<(usize, &FreeSpot)> {
        for (i, spot) in self.unallocated.iter().enumerate() {
            if spot.size <= size {
                return Some((i, spot))
            }
        }

        None
    }

    fn findFreeIndex(&self, offset: &FreeSpot) -> usize {
        for (i, item) in self.unallocated.iter().enumerate() {
            if offset.index + offset.size < item.index {
                return i;
            }
        }
        return self.unallocated.len()-1;
    }
}