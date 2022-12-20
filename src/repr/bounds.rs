prelude!();

#[derive(Debug, Clone, Copy)]
pub struct BoundedColl {
    pub pref: usize,
    pub tail: bool,
    pub tail_bound: Option<usize>,
}
impl BoundedColl {
    pub fn exact(len: usize) -> Self {
        Self {
            pref: len,
            tail: false,
            tail_bound: None,
        }
    }

    pub fn with_tail(mut self, ubound: Option<usize>) -> Self {
        self.tail = true;
        self.tail_bound = ubound;
        self
    }
}

/// Reference bounds.
///
/// # Invariants
///
/// - if `let Some(ubound) = self.ubound`, then `lbound ≤ ubound`
#[derive(Debug, Clone, Copy)]
pub struct Bounds {
    pub lbound: usize,
    pub ubound: Option<usize>,
}

impl Display for Bounds {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, ", self.lbound)?;
        if let Some(ubound) = self.ubound {
            write!(f, "{}]", ubound)?;
        } else {
            write!(f, "∞[")?;
        }
        Ok(())
    }
}

impl Bounds {
    pub fn new(lbound: usize, ubound: Option<usize>) -> Res<Self> {
        if let Some(ubound) = ubound {
            if ubound < lbound {
                bail!(
                    "illegal bounds, lower bound ({lbound}) must less than \
                    or equal to upper bound ({ubound})"
                )
            }
        }
        Ok(Self { lbound, ubound })
    }

    /// Parses some bounds.
    ///
    /// The default for `lbound` is `1`, and the default for `ubound` is `max(lbound, 1)`.
    ///
    /// If you're parsing bounds for a structural feature, use
    /// [`repr::structural::Typ::parse_bounds`] instead.
    pub fn from_str(lbound: Option<&str>, ubound: Option<&str>) -> Res<Self> {
        let lbound = match lbound {
            None => 1,
            Some(lbound) => {
                if let Some(lbound) = usize::from_str_radix(lbound, 10).ok() {
                    lbound
                } else {
                    bail!(@unexpected("lower bound (unsigned integer)") lbound)
                }
            }
        };
        let ubound = match ubound {
            None => Some(if lbound == 0 { 1 } else { lbound }),
            Some("-1") => None,
            Some(ubound) => {
                if let Some(ubound) = usize::from_str_radix(ubound, 10).ok() {
                    Some(ubound)
                } else {
                    bail!(@unexpected("upper bound (`-1` or unsigned integer)") ubound)
                }
            }
        };
        Self::new(lbound, ubound)
    }

    pub fn get_exact(self) -> Option<usize> {
        if self.ubound == Some(self.lbound) {
            self.ubound
        } else {
            None
        }
    }

    pub fn is_empty(self) -> bool {
        self.get_exact() == Some(0)
    }

    pub fn to_coll(self) -> BoundedColl {
        let mut coll = BoundedColl::exact(self.lbound);
        if self.ubound != Some(self.lbound) {
            coll = coll.with_tail(self.ubound)
        }
        coll
    }
}
