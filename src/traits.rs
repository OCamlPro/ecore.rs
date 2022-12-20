prelude! {
    repr::*,
}

pub trait HasAnnots {
    fn annotations(&self) -> &Annots;
    fn annotations_mut(&mut self) -> &mut Annots;
}

pub trait HasStructural {
    fn structural(&self) -> &[repr::Structural];
    fn structural_mut(&mut self) -> &mut Vec<repr::Structural>;
    fn structural_push(&mut self, s: impl Into<repr::Structural>) {
        self.structural_mut().push(s.into())
    }
}
