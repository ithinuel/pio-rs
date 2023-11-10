pub trait Storage<Item>: IntoIterator<Item = Item> + FromIterator<Item> + Default {
    fn get_mut(&mut self, index: usize) -> Option<&mut Item>;
    fn push(&mut self, element: Item);
    fn len(&self) -> usize;
}

#[cfg(feature = "std")]
impl<T> Storage<T> for Vec<T> {
    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.as_mut_slice().get_mut(index)
    }
    fn push(&mut self, element: T) {
        self.push(element)
    }
    fn len(&self) -> usize {
        self.len()
    }
}
#[cfg(feature = "arrayvec")]
impl<T, const SIZE: usize> Storage<T> for ArrayVec<T, SIZE> {
    fn get_mut(&mut self, index: usize) -> Option<&mut u16> {
        self.as_mut_slice().get_mut(index)
    }
    fn push(&mut self, element: T) {
        self.push(element)
    }
    fn len(&self) -> usize {
        self.len()
    }
}
#[cfg(feature = "heapless")]
impl<T, const SIZE: usize> Storage<T> for Vec<T, SIZE> {
    fn get_mut(&mut self, index: usize) -> Option<&mut u16> {
        self.as_mut_slice().get_mut(index)
    }
    fn push(&mut self, element: T) {
        self.push(element)
    }
    fn len(&self) -> usize {
        self.len()
    }
}
