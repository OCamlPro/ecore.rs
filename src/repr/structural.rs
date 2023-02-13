prelude!(repr::bounds::Bounds);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Typ {
    EReference,
    EAttribute,
}

impl Display for Typ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EReference => "EReference".fmt(f),
            Self::EAttribute => "EAttribute".fmt(f),
        }
    }
}

impl Typ {
    pub fn from_xsi_type(s: impl AsRef<str>) -> Res<Self> {
        let s = s.as_ref();
        match s {
            "ecore:EAttribute" => Ok(Self::EAttribute),
            "ecore:EReference" => Ok(Self::EReference),
            _ => bail!("unexpected structural feature `xsi:type` value `{}`", s),
        }
    }

    pub fn parse_bounds(self, lbound: Option<&str>, ubound: Option<&str>) -> Res<Bounds> {
        let lbound = match (lbound, self) {
            (Some(lbound), _) => lbound.as_ref(),
            (None, Self::EReference) => "0",
            (None, Self::EAttribute) => "1",
        };
        Bounds::from_str(Some(lbound), ubound)
    }
}

#[derive(Debug, Clone)]
pub struct Structural {
    pub name: String,
    pub kind: Typ,
    pub typ: idx::Class,
    pub bounds: Bounds,
    pub annotations: repr::Annots,
    /// No idea what this is, corresponds to the attribute `containment`.
    pub containment: bool,
    /// No idea what this is, corresponds to the attribute `iD`.
    pub is_id: bool,
}
impl HasAnnots for Structural {
    fn annotations(&self) -> &repr::Annots {
        &self.annotations
    }
    fn annotations_mut(&mut self) -> &mut repr::Annots {
        &mut self.annotations
    }
}
impl Structural {
    pub fn new(name: impl Into<String>, kind: Typ, typ: idx::Class, bounds: Bounds) -> Res<Self> {
        let name = name.into();
        if !is_valid_ident(&name) {
            bail!("illegal structural feature identifier `{}`", name)
        }
        Ok(Self {
            name,
            kind,
            typ,
            bounds,
            annotations: repr::Annots::with_capacity(3),
            containment: false,
            is_id: false,
        })
    }

    pub fn set_containment(&mut self, flag: bool) {
        self.containment = flag
    }
    pub fn try_set_containment(&mut self, flag: Option<bool>) {
        if let Some(flag) = flag {
            self.set_containment(flag)
        }
    }
    pub fn set_is_id(&mut self, flag: bool) {
        self.is_id = flag
    }
    pub fn try_set_is_id(&mut self, flag: Option<bool>) {
        if let Some(flag) = flag {
            self.set_is_id(flag)
        }
    }
}
