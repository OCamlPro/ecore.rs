//! Builtin Ecore classes/types.

prelude! {
    repr::*,
}

pub type Builder = fn(Path, idx::Class) -> Class;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Typ {
    Object,
    EByte,
    EShort,
    EInt,
    ELong,
    EFloat,
    EDouble,
    EBoolean,
    EChar,
    EString,
}
impl AsRef<Typ> for Typ {
    fn as_ref(&self) -> &Typ {
        self
    }
}

impl Display for Typ {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Typ::*;
        match self {
            Object => "Object".fmt(fmt),
            EByte => "EByte".fmt(fmt),
            EShort => "EShort".fmt(fmt),
            EInt => "EInt".fmt(fmt),
            ELong => "ELong".fmt(fmt),
            EFloat => "EFloat".fmt(fmt),
            EDouble => "EDouble".fmt(fmt),
            EBoolean => "EBoolean".fmt(fmt),
            EChar => "EChar".fmt(fmt),
            EString => "EString".fmt(fmt),
        }
    }
}

impl Typ {
    const ECORE_BUILTIN: &'static str = "ecore:Builtin";
}

macro_rules! builtin_builders {
    ( $(
        $builder_name:ident = $variant:ident
    ),* $(,)? ) => {
        impl Typ {
            $(
                fn $builder_name() -> (Typ, Builder) {
                    (Typ::$variant, |path, idx| {
                        Class::new(
                            idx,
                            path,
                            Typ::ECORE_BUILTIN,
                            stringify!($variant),
                            None as Option<String>,
                            Some(false),
                            Some(false),
                        )
                    })
                }
            )*

            /// Lists all builtin types and their corresponding class builder.
            pub fn builders() -> impl IntoIterator<Item = (Typ, Builder)> {
                [
                    $( Self::$builder_name() , )*
                ].into_iter()
            }

            /// Parses a builtin type URL appearing after a `ecore:EDataType` in an `eType` attribute.
            pub fn parse_etype_url(s: impl AsRef<str>) -> Res<Self> {
                let s = s.as_ref();
                match s {
                    $(
                        concat!("http://www.eclipse.org/emf/2002/Ecore#//", stringify!($variant)) =>
                            Ok(Self::$variant),
                    )*
                    _ => bail!("unknown builtit class URL `{}`", s),
                }
            }
        }
    };
}

builtin_builders! {
    build_object = Object,
    build_byte = EByte,
    build_short = EShort,
    build_int = EInt,
    build_long = ELong,
    build_float = EFloat,
    build_double = EDouble,
    build_bool = EBoolean,
    build_char = EChar,
    build_string = EString,
}

impl Typ {
    /// Recognizes a builtin `ecore` datatype from its `eType` description.
    ///
    /// Returns `None` if `s` does not start with `ecore:EDataType`, otherwise returns the type if it is
    /// recognized from the URL part and an error if not.
    ///
    /// If you know you're parsing a builtin type, like if you already parsed the `ecore:EDataType`
    /// header of the `eType`, use [`Self::parse_etype_url`] instead.
    pub fn try_parse_etype(s: impl AsRef<str>) -> Res<Option<Self>> {
        let s = s.as_ref();
        let pref = "ecore:EDataType";
        if !s.starts_with(pref) {
            return Ok(None);
        }
        let s = s[pref.len()..].trim();
        Self::parse_etype_url(s).map(Some)
    }
}
