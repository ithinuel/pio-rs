pub trait Storage<Item>: IntoIterator<Item = Item> {
    fn get_mut(&mut self, index: usize) -> Option<&mut Item>;
    fn len(&self) -> usize;
}
pub trait WritableStorage<Item>: Storage<Item> {
    fn push(&mut self, element: Item);
}

impl<const SZ: usize, T> Storage<T> for [T; SZ] {
    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index >= SZ {
            None
        } else {
            Some(&mut self[index])
        }
    }

    fn len(&self) -> usize {
        SZ
    }
}

#[cfg(feature = "std")]
mod std {
    use super::{Storage, WritableStorage};
    impl<T> Storage<T> for Vec<T> {
        fn get_mut(&mut self, index: usize) -> Option<&mut T> {
            self.as_mut_slice().get_mut(index)
        }
        fn len(&self) -> usize {
            self.len()
        }
    }
    impl<T> WritableStorage<T> for Vec<T> {
        fn push(&mut self, element: T) {
            self.push(element)
        }
    }
}
#[cfg(feature = "arrayvec")]
mod arrayvec {
    use super::{Storage, WritableStorage};
    impl<T, const SIZE: usize> Storage<T> for arrayvec::ArrayVec<T, SIZE> {
        fn get_mut(&mut self, index: usize) -> Option<&mut u16> {
            self.as_mut_slice().get_mut(index)
        }
        fn len(&self) -> usize {
            self.len()
        }
    }
    impl<T, const SIZE: usize> WritableStorage<T> for ArrayVec<T, SIZE> {
        fn push(&mut self, element: T) {
            self.push(element)
        }
    }
}
#[cfg(feature = "heapless")]
mod heapless {
    use super::{Storage, WritableStorage};
    impl<T, const SIZE: usize> Storage<T> for heapless::Vec<T, SIZE> {
        fn get_mut(&mut self, index: usize) -> Option<&mut u16> {
            self.as_mut_slice().get_mut(index)
        }
        fn len(&self) -> usize {
            self.len()
        }
    }
    impl<T, const SIZE: usize> WritableStorage<T> for Vec<T, SIZE> {
        fn push(&mut self, element: T) {
            self.push(element)
        }
    }
}
