/// Imports this crate's prelude.
#[macro_export]
macro_rules! prelude {
    ($($imports:tt)*) => (
        use $crate::prelude::{*, $($imports)*};
    )
}

pub use std::{
    borrow::{Borrow, Cow},
    collections::{BTreeSet, HashMap},
    fmt::{self, Display},
    mem,
};

pub use log;
pub use regex;
pub use smallvec::{smallvec, SmallVec};

#[macro_use]
pub mod res;

pub use crate::{
    bail,
    ctx::{self, Ctx},
    error, parser,
    prelude::{
        path_map::PathMap,
        res::{Res, WithCtx},
    },
    repr::{self, builtin, idx},
    traits::*,
};

mod path_map {
    prelude! {
        repr::Path,
    }

    pub struct PathMap<K, V>
    where
        K: std::hash::Hash + Eq,
    {
        map: HashMap<Path, HashMap<K, V>>,
    }

    impl<K, V> std::ops::Deref for PathMap<K, V>
    where
        K: std::hash::Hash + Eq,
    {
        type Target = HashMap<Path, HashMap<K, V>>;
        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }
    impl<K, V> std::ops::DerefMut for PathMap<K, V>
    where
        K: std::hash::Hash + Eq,
    {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.map
        }
    }

    impl<K, V> PathMap<K, V>
    where
        K: std::hash::Hash + Eq,
    {
        pub fn with_capacity(capa: usize) -> Self {
            Self {
                map: HashMap::with_capacity(capa),
            }
        }
        pub fn new() -> Self {
            Self::with_capacity(3)
        }

        pub fn at(&self, path: &Path) -> Option<&HashMap<K, V>> {
            self.map.get(path)
        }
        pub fn at_mut(&mut self, path: &Path) -> Option<&mut HashMap<K, V>> {
            self.map.get_mut(path)
        }
        pub fn at_mut_or_new(&mut self, path: Path) -> &mut HashMap<K, V> {
            self.map.entry(path).or_insert_with(HashMap::new)
        }

        pub fn unwrap_at(
            &self,
            path: &Path,
            display: impl FnOnce(&Path) -> String,
        ) -> Res<&HashMap<K, V>> {
            self.map
                .get(path)
                .ok_or_else(|| error!(@unknown("package path") display(path)))
        }
        pub fn unwrap_at_mut(
            &mut self,
            path: &Path,
            display: impl FnOnce(&Path) -> String,
        ) -> Res<&mut HashMap<K, V>> {
            self.map
                .get_mut(path)
                .ok_or_else(|| error!(@unknown("package path") display(path)))
        }
    }
}

pub trait CollPrintExt: Sized {
    type Elm;
    fn show_iter<S>(self, show_elm: impl Fn(Self::Elm) -> S, sep: impl AsRef<str>) -> String
    where
        S: AsRef<str>;

    fn show_iter_cs<S>(self, show_elm: impl Fn(Self::Elm) -> S) -> String
    where
        S: AsRef<str>,
    {
        self.show_iter(show_elm, ", ")
    }
}
impl<'a, T, E> CollPrintExt for T
where
    T: IntoIterator<Item = E>,
{
    type Elm = E;
    fn show_iter<S>(self, show_elm: impl Fn(Self::Elm) -> S, sep: impl AsRef<str>) -> String
    where
        S: AsRef<str>,
    {
        let sep = sep.as_ref();
        let mut s = String::new();
        for elm in self {
            if !s.is_empty() {
                s.push_str(sep);
            }
            s.push_str(show_elm(elm).as_ref())
        }
        s
    }
}
