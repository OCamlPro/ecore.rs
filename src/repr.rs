prelude! {}

pub mod bounds;
pub mod builtin;
pub mod idx;
pub mod structural;

pub use bounds::Bounds;
pub use structural::Structural;

pub mod annot {
    pub type Source = String;
    pub type Key = String;
    pub type Val = String;
}

/// A package/class annotation.
///
/// Unlike packages/classes, annotations are not stored by the factory/context. They just appear
/// as-is in the package/class they annotate. Maybe that's not a good idea though.
#[derive(Debug, Clone)]
pub struct Annot {
    source: annot::Source,
    details: HashMap<annot::Key, annot::Val>,
}

pub type Annots = Vec<Annot>;

impl std::ops::Deref for Annot {
    type Target = HashMap<annot::Key, annot::Val>;
    fn deref(&self) -> &Self::Target {
        &self.details
    }
}

impl Annot {
    pub fn with_capacity(source: impl Into<annot::Source>, details_capa: usize) -> Self {
        Self {
            source: source.into(),
            details: HashMap::with_capacity(details_capa),
        }
    }

    pub fn shrink_to_fit(&mut self) {
        self.details.shrink_to_fit()
    }

    pub fn source(&self) -> &str {
        &self.source
    }
    pub fn details(&self) -> &HashMap<annot::Key, annot::Val> {
        &self.details
    }

    /// Same as [`HashMap`]'s `insert` function, but fails with context on overwrite.
    pub fn insert(&mut self, key: impl Into<annot::Key>, val: impl Into<annot::Val>) -> Res<()> {
        let entry = self.details.entry(key.into());
        use std::collections::hash_map::Entry::*;
        match entry {
            Vacant(entry) => {
                entry.insert(val.into());
            }
            Occupied(entry) => {
                let val = val.into();
                let err = error!(@redef("annotation details key/value binding") entry.key())
                    .with_context(format!(
                        "trying to replace value `{}` with `{}`",
                        entry.get(),
                        val
                    ));
                return Err(err);
            }
        }
        Ok(())
    }
}

/// A sequence of package indices, understood as an **absolute** path.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path {
    last: idx::Pack,
    path: SmallVec<[idx::Pack; 8]>,
}

impl Path {
    pub fn new(idx: idx::Pack) -> Self {
        Self {
            last: idx,
            path: smallvec![],
        }
    }

    /// Constructs the path corresponding to a package in the context.
    pub fn of_idx(ctx: &Ctx, idx: idx::Pack) -> Self {
        let last = idx;
        let mut rev_path = smallvec!();

        let mut current = &ctx[idx];

        while let Some(parent) = current.sup() {
            rev_path.push(parent);
            current = &ctx[parent];
        }
        rev_path.reverse();
        let path = rev_path;
        Self { last, path }
    }

    pub fn iter<'me>(&'me self) -> impl Iterator<Item = idx::Pack> + 'me {
        self.path.iter().cloned().chain(Some(self.last).into_iter())
    }
    pub fn iter_pref<'me>(&'me self) -> impl Iterator<Item = idx::Pack> + 'me {
        self.path.iter().cloned()
    }

    pub fn len(&self) -> usize {
        self.pref_len() + 1
    }
    pub fn pref_len(&self) -> usize {
        self.path.len()
    }

    pub fn push(&mut self, idx: idx::Pack) {
        let to_push = mem::replace(&mut self.last, idx);
        self.path.push(to_push)
    }
    pub fn pop(&mut self) -> Option<idx::Pack> {
        self.path
            .pop()
            .map(|new_last| mem::replace(&mut self.last, new_last))
    }
    pub fn last(&self) -> idx::Pack {
        self.last
    }
    pub fn first(&self) -> idx::Pack {
        self.path.get(0).cloned().unwrap_or(self.last)
    }

    /// Suports forward-referencing.
    fn resolve_relative_etype(
        mut path: Path,
        ctx: &mut Ctx,
        s: impl AsRef<str>,
    ) -> Res<idx::Class> {
        let s = s.as_ref();
        let mut bits = s.split("/").into_iter();
        let mut next = bits.next();
        let mut last = None;

        while let Some(ident) = next {
            next = bits.next();

            if next.is_some() {
                let p_idx = ctx.pack_idx_or_forward_ref(&path, ident)?;
                path.push(p_idx);
            } else {
                last = Some(ident);
                break;
            }
        }

        if let Some(ident) = last {
            ctx.get_class_idx_or_forward_ref_in(&path, ident)
        } else {
            bail!("illegal empty absolute `eType`: `{}`", s);
        }
    }

    /// Resolves the value of an `eType` XML attribute.
    ///
    /// This function handles forward referencing, hence the mutable context.
    pub fn resolve_etype(ctx: &mut ctx::PathCtx, s: impl AsRef<str>) -> Res<idx::Class> {
        let s = s.as_ref();
        if let Some(idx) = ctx.ctx().try_parse_builtin_etype(s)? {
            return Ok(idx);
        }

        let rel_pref = "#//";
        if s.starts_with(rel_pref) {
            let s = &s[rel_pref.len()..];
            Self::resolve_relative_etype(ctx.path().clone(), ctx.ctx_mut(), s)
        } else {
            bail!("unsupported `eType` path `{}`", s);
        }
    }

    pub fn display(&self, ctx: &idx::PackMap<Pack>) -> String {
        let mut res = self.path.iter().fold(String::new(), |mut s, p_idx| {
            s.push_str(ctx[*p_idx].name());
            s.push_str("::");
            s
        });
        res.push_str(ctx[self.last].name());
        res
    }
    pub fn display_sep(&self, ctx: &idx::PackMap<Pack>) -> String {
        let mut res = self.display(ctx);
        res.push_str("::");
        res
    }
}

/// A literal, which are used to specify enum variants.
#[derive(Debug, Clone)]
pub struct ELit {
    name: String,
    value: Option<String>,
}

pub type ELits = Vec<ELit>;

impl ELit {
    pub fn new(name: impl Into<String>, value: Option<impl Into<String>>) -> Self {
        Self {
            name: name.into(),
            value: value.map(|s| s.into()),
        }
    }
    pub fn new_name(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: None,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn value(&self) -> Option<&str> {
        self.value.as_ref().map(|s| s.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    name: String,
    bounds: Bounds,
    typ: idx::Class,
}

pub type Params = Vec<Param>;

impl Param {
    pub fn new(name: impl Into<String>, bounds: impl Into<Bounds>, typ: idx::Class) -> Self {
        Self {
            name: name.into(),
            bounds: bounds.into(),
            typ,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn bounds(&self) -> &Bounds {
        &self.bounds
    }
    pub fn typ(&self) -> idx::Class {
        self.typ
    }
}

#[derive(Debug, Clone)]
pub struct Operation {
    name: String,
    typ: Option<idx::Class>,
    parameters: Params,
}

pub type Operations = Vec<Operation>;

impl Operation {
    pub fn with_capacity(
        name: impl Into<String>,
        typ: Option<idx::Class>,
        param_capa: usize,
    ) -> Self {
        Self {
            name: name.into(),
            typ: typ,
            parameters: Params::with_capacity(param_capa),
        }
    }
    pub fn new(name: impl Into<String>, typ: Option<idx::Class>) -> Self {
        Self::with_capacity(name, typ, 3)
    }
    pub fn add_parameter(&mut self, param: Param) {
        self.parameters.push(param)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn typ(&self) -> Option<idx::Class> {
        self.typ
    }
    pub fn parameters(&self) -> &Params {
        &self.parameters
    }
}

#[derive(Debug)]
pub struct Class {
    pub idx: idx::Class,
    pub path: Path,
    concrete: bool,
    is_interface: bool,
    typ: String,
    name: String,
    inst_name: Option<String>,
    literals: Vec<ELit>,
    annotations: Annots,
    sup: BTreeSet<idx::Class>,
    sub: BTreeSet<idx::Class>,
    structural: Vec<Structural>,
    operations: Operations,
}

impl HasAnnots for Class {
    fn annotations(&self) -> &Annots {
        &self.annotations
    }
    fn annotations_mut(&mut self) -> &mut Annots {
        &mut self.annotations
    }
}
impl HasStructural for Class {
    fn structural(&self) -> &[Structural] {
        &self.structural
    }
    fn structural_mut(&mut self) -> &mut Vec<repr::Structural> {
        &mut self.structural
    }
}

impl Class {
    pub fn new(
        idx: idx::Class,
        path: Path,
        typ: impl Into<String>,
        name: impl Into<String>,
        inst_name: Option<impl Into<String>>,
        is_abstract: Option<bool>,
        is_interface: Option<bool>,
    ) -> Self {
        Self {
            idx,
            path,
            typ: typ.into(),
            name: name.into(),
            inst_name: inst_name.map(Into::into),
            concrete: !is_abstract.unwrap_or(false),
            is_interface: is_interface.unwrap_or(false),
            literals: ELits::with_capacity(7),
            annotations: Annots::with_capacity(3),
            sup: BTreeSet::new(),
            sub: BTreeSet::new(),
            structural: Vec::with_capacity(5),
            operations: Operations::with_capacity(7),
        }
    }

    pub fn shrink_to_fit(&mut self) {
        // keep this so that we know when a new field is added, and update this thing
        let Self {
            idx: _,
            path: _,
            typ: _,
            name: _,
            inst_name: _,
            concrete: _,
            is_interface: _,
            literals,
            annotations,
            sup: _,
            sub: _,
            structural,
            operations,
        } = self;
        literals.shrink_to_fit();
        annotations.shrink_to_fit();
        structural.shrink_to_fit();
        operations.shrink_to_fit();
    }

    #[inline]
    pub fn is_concrete(&self) -> bool {
        self.concrete
    }
    #[inline]
    pub fn is_interface(&self) -> bool {
        self.is_interface
    }
    #[inline]
    pub fn is_abstract(&self) -> bool {
        !self.is_concrete()
    }
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }
    #[inline]
    pub fn inst_name(&self) -> Option<&str> {
        self.inst_name.as_ref().map(|s| s.as_str())
    }

    pub fn annotations(&self) -> &Annots {
        &self.annotations
    }
    pub fn add_annotation(&mut self, annot: Annot) {
        self.annotations.push(annot)
    }

    pub fn literals(&self) -> &ELits {
        &self.literals
    }
    pub fn add_literal(&mut self, lit: ELit) {
        self.literals.push(lit)
    }

    pub fn operations(&self) -> &Operations {
        &self.operations
    }
    pub fn add_operation(&mut self, op: Operation) {
        self.operations.push(op)
    }

    pub fn typ(&self) -> &String {
        &self.typ
    }

    pub fn sup(&self) -> &BTreeSet<idx::Class> {
        &self.sup
    }
    pub fn add_sup(&mut self, sup: idx::Class) -> bool {
        self.sup.insert(sup)
    }

    pub fn sub(&self) -> &BTreeSet<idx::Class> {
        &self.sub
    }
    pub fn add_sub(&mut self, sub: idx::Class) -> bool {
        self.sub.insert(sub)
    }

    pub fn structural(&self) -> &[Structural] {
        &self.structural
    }
    pub fn add_structural(&mut self, val: impl Into<Structural>) {
        self.structural.push(val.into())
    }

    pub fn display(&self, ctx: &Ctx) -> String {
        format!("{}/{}", self.path.display(ctx.packs()), self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Pack {
    pub idx: idx::Pack,
    sup: Option<idx::Pack>,
    sub: BTreeSet<idx::Pack>,
    name: String,
    classes: BTreeSet<idx::Class>,
    annotations: Annots,
}

impl Pack {
    pub fn with_capacity(idx: idx::Pack, name: impl Into<String>, sup: Option<idx::Pack>) -> Self {
        Self {
            idx,
            sup,
            sub: BTreeSet::new(),
            name: name.into(),
            classes: BTreeSet::new(),
            annotations: Annots::with_capacity(3),
        }
    }
    pub fn new(idx: idx::Pack, name: impl Into<String>, sup: Option<idx::Pack>) -> Self {
        Self::with_capacity(idx, name, sup)
    }

    /// A package is empty if it has nothing but an index, a name, and optionnaly a `sup`.
    pub fn is_empty(&self) -> bool {
        self.sub.is_empty() && self.classes.is_empty() && self.annotations.is_empty()
    }

    pub fn path(&self, ctx: &Ctx) -> Path {
        Path::of_idx(ctx, self.idx)
    }

    pub fn annotations(&self) -> &Annots {
        &self.annotations
    }
    pub fn add_annotation(&mut self, annot: Annot) {
        self.annotations.push(annot)
    }

    pub fn sup(&self) -> Option<idx::Pack> {
        self.sup
    }
    pub fn set_sup(&mut self, sup: idx::Pack) -> Option<idx::Pack> {
        mem::replace(&mut self.sup, Some(sup))
    }
    pub fn has_sup(&mut self, sup: idx::Pack) -> bool {
        self.sup == Some(sup)
    }

    pub fn sub(&self) -> &BTreeSet<idx::Pack> {
        &self.sub
    }
    pub fn add_sub(&mut self, sub: idx::Pack) -> bool {
        self.sub.insert(sub)
    }
    pub fn has_sub(&self, sub: idx::Pack) -> bool {
        self.sub.contains(&sub)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn classes(&self) -> &BTreeSet<idx::Class> {
        &self.classes
    }
    pub fn classes_insert(&mut self, idx: idx::Class) -> bool {
        self.classes.insert(idx)
    }
}
