//! Result-handling types/traits/macros.

prelude! {}

#[macro_export]
macro_rules! error {
    ( $fmt_head:literal $(, $fmt_args:expr)* $(,)? ) => {
        $crate::prelude::res::Error::from(format!(
            $fmt_head $(, $fmt_args)*
        ))
    };
    ( @unknown($desc:expr) $val:expr ) => {
        $crate::prelude::res::Error::from(
            $crate::prelude::res::ErrorSrc::new_unknown($val, $desc)
        )
    };
    ( @redef($desc:expr) $val:expr ) => {
        $crate::prelude::res::Error::from(
            $crate::prelude::res::ErrorSrc::new_redef($val, $desc)
        )
    };
    ( @unexpected($desc:expr) $val:expr ) => {
        $crate::prelude::res::Error::from(
            $crate::prelude::res::ErrorSrc::new_unexpected($val, $desc)
        )
    };
    ($e:expr) => {
        $crate::prelude::res::Error::from(
            $crate::prelude::res::ErrorSrc::from($e)
        )
    };
    ($($unexpected:tt)*) => {
        compile_error!(
            "expected println!-like string interpolation \
            or an expression convertible `Into` an error"
        )
    };
}

#[macro_export]
macro_rules! bail {
    ($($input:tt)+) => {
        return $crate::prelude::Res::Err($crate::error!($($input)+))
    };
}

#[derive(Debug)]
pub enum ErrorSrc {
    Msg(String),
    Unknown {
        val: String,
        /// Could be a `&'static str` depending on how we use it.
        desc: String,
    },
    Redef {
        val: String,
        /// Could be a `&'static str` depending on how we use it.
        desc: String,
    },
    Unexpected {
        val: String,
        desc: String,
    },
    Std(Box<dyn std::error::Error>),
    ParserError(parser::cooked::OwningError),
}
impl ErrorSrc {
    pub fn new_msg(s: impl Into<String>) -> Self {
        Self::Msg(s.into())
    }
    pub fn new_std(e: impl std::error::Error + 'static) -> Self {
        Self::Std(Box::new(e))
    }
    pub fn new_unknown(val: impl Into<String>, desc: impl Into<String>) -> Self {
        Self::Unknown {
            val: val.into(),
            desc: desc.into(),
        }
    }
    pub fn new_redef(val: impl Into<String>, desc: impl Into<String>) -> Self {
        Self::Redef {
            val: val.into(),
            desc: desc.into(),
        }
    }
    pub fn new_unexpected(val: impl Into<String>, desc: impl Into<String>) -> Self {
        Self::Unexpected {
            val: val.into(),
            desc: desc.into(),
        }
    }
}
impl From<String> for ErrorSrc {
    fn from(s: String) -> Self {
        Self::Msg(s)
    }
}
impl From<&'_ str> for ErrorSrc {
    fn from(s: &str) -> Self {
        Self::Msg(s.into())
    }
}
impl From<std::io::Error> for ErrorSrc {
    fn from(s: std::io::Error) -> Self {
        Self::new_std(s)
    }
}
impl<'a> From<parser::cooked::Error<'a>> for ErrorSrc {
    fn from(value: parser::cooked::Error<'a>) -> Self {
        Self::ParserError(value.into_owning())
    }
}

impl Display for ErrorSrc {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ErrorSrc::*;
        match self {
            Msg(blah) => {
                for (idx, line) in blah.lines().enumerate() {
                    if idx > 0 {
                        writeln!(fmt)?;
                    }
                    line.fmt(fmt)?;
                }
                Ok(())
            }
            Unknown { val, desc } => {
                write!(fmt, "unknown {} `{}`", desc, val)
            }
            Redef { val, desc } => {
                write!(fmt, "illegal redefinition of {} `{}`", desc, val)
            }
            Unexpected { val, desc } => {
                write!(fmt, "unexpected {} `{}`", desc, val)
            }
            Std(err) => err.fmt(fmt),
            ParserError(err) => write!(fmt, "{err}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorCtx {
    Msg(String),
}
impl ErrorCtx {
    pub fn new_msg(s: impl Into<String>) -> Self {
        Self::Msg(s.into())
    }
}
impl From<String> for ErrorCtx {
    fn from(s: String) -> Self {
        Self::new_msg(s)
    }
}
impl From<&'_ str> for ErrorCtx {
    fn from(s: &str) -> Self {
        Self::new_msg(s)
    }
}
impl Display for ErrorCtx {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorCtx::*;
        match self {
            Msg(blah) => {
                for (idx, line) in blah.lines().enumerate() {
                    if idx > 0 {
                        writeln!(fmt)?;
                    }
                    line.fmt(fmt)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Error {
    pub src: ErrorSrc,
    pub ctx: Vec<ErrorCtx>,
}
impl<T> From<T> for Error
where
    T: Into<ErrorSrc>,
{
    fn from(t: T) -> Self {
        Self {
            src: t.into(),
            ctx: vec![],
        }
    }
}
impl Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.src.fmt(fmt)?;
        for ctx in self.ctx.iter() {
            writeln!(fmt)?;
            ctx.fmt(fmt)?;
        }
        Ok(())
    }
}

pub type Res<T> = Result<T, Error>;

pub trait WithCtx: Sized {
    type Output;
    fn context<E>(self, ctx: impl FnOnce() -> E) -> Self::Output
    where
        E: Into<ErrorCtx>;
    fn with_context(self, ctx: impl Into<ErrorCtx>) -> Self::Output {
        self.context(|| ctx)
    }
}

impl WithCtx for ErrorSrc {
    type Output = Error;
    fn context<E>(self, ctx: impl FnOnce() -> E) -> Self::Output
    where
        E: Into<ErrorCtx>,
    {
        Error {
            src: self,
            ctx: vec![ctx().into()],
        }
    }
}
impl WithCtx for Error {
    type Output = Error;
    fn context<E>(mut self, ctx: impl FnOnce() -> E) -> Self::Output
    where
        E: Into<ErrorCtx>,
    {
        self.ctx.push(ctx().into());
        self
    }
}
impl<T, E> WithCtx for Result<T, E>
where
    E: Into<Error>,
{
    type Output = Res<T>;
    fn context<Er>(self, ctx: impl FnOnce() -> Er) -> Self::Output
    where
        Er: Into<ErrorCtx>,
    {
        self.map_err(|err| err.into().context(ctx))
    }
}
