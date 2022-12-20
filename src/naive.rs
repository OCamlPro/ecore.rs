pub trait PersonSpec {
    fn name(&self) -> &String;
    fn set_name(&mut self, name: impl Into<String>);
}

#[derive(Debug, Clone, Hash)]
pub enum Person {
    Student(Student),
    Teacher(Teacher),
}
impl PersonSpec for Person {
    fn name(&self) -> &String {
        match self {
            Self::Student(s) => s.name(),
            Self::Teacher(t) => t.name(),
        }
    }
    fn set_name(&mut self, name: impl Into<String>) {
        match self {
            Self::Student(s) => s.set_name(name),
            Self::Teacher(t) => t.set_name(name),
        }
    }
}

pub trait ResearcherSpec {
    fn manages(&self) -> &Vec<Researcher>;
}

#[derive(Debug, Clone, Hash)]
pub enum Researcher {
    Student(Student),
    Teacher(Teacher),
}
impl ResearcherSpec for Researcher {
    fn manages(&self) -> &Vec<Researcher> {
        match self {
            Self::Student(s) => s.manages(),
            Self::Teacher(t) => t.manages(),
        }
    }
}

type Address = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StudentKind {
    Master,
    PhD,
}

#[derive(Debug, Clone, Hash)]
pub struct Student {
    kind: StudentKind,
    id: String,
    advisor: Teacher,
    /// For [`PersonSpec`]'s notion of name.
    name: String,
    /// For [`ResearcherSpec`]'s notion of managed researchers.
    manages: Vec<Researcher>,
}
impl PersonSpec for Student {
    fn name(&self) -> &String {
        &self.name
    }
    fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into()
    }
}
impl ResearcherSpec for Student {
    fn manages(&self) -> &Vec<Researcher> {
        &self.manages
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Teacher {
    address: Address,
    /// For [`PersonSpec`]'s notion of name.
    name: String,
    /// For [`ResearcherSpec`]'s notion of name.
    manages: Vec<Researcher>,
}
impl PersonSpec for Teacher {
    fn name(&self) -> &String {
        &self.name
    }
    fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into()
    }
}
impl ResearcherSpec for Teacher {
    fn manages(&self) -> &Vec<Researcher> {
        &self.manages
    }
}
