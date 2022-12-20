//! Parsing context.
//!
//! The main context is [`Ctx`], which can turn itself into a [`PathCtx`] which is really a [`Ctx`]
//! along with a package [`Path`]. [`PathCtx`]s allow to act on the current package directly during
//! parsing to add annotations, go down a sub-package (yielding a new [`PathCtx`]), or go up the
//! parent package.
//!
//! To add a class to a [`PathCtx`], one first *"enters the class"* by calling
//! [`PathCtx::enter_class`] which yields a [`ClassCtx`]. A class context in turn allows adding
//! annotations, operations, structural features... to the current class before finalizing it
//! and going back to the [`PathCtx`] that spawned it.

prelude! {
    repr::{Class, Pack, Path},
}

/// A plain context, see also [`PathCtx`] and [`ClassCtx`].
///
/// Note that this type needs to be [finalized](Self::finalize).
///
/// # Forward Reference
///
/// Ecore allows classes to be forward referenced, in particular in structural features. To handle
/// this, we have maintain `forward_refs`, a set of pre-declared classes that must be defined before
/// parsing [finalization](Self::finalize).
///
/// When registering a class, the context will automatically update `forward_refs`.
pub struct Ctx {
    packs: idx::PackMap<Pack>,
    top_pack: idx::Pack,
    builtin_pack: idx::Pack,
    name_to_pack: PathMap<String, idx::Pack>,
    classes: idx::ClassMap<Class>,
    name_to_class: PathMap<String, idx::Class>,
    builtin_map: HashMap<builtin::Typ, idx::Class>,
    forward_ref_classes: BTreeSet<idx::Class>,
    forward_ref_packs: BTreeSet<idx::Pack>,
}

impl std::ops::Index<idx::Class> for Ctx {
    type Output = Class;
    fn index(&self, idx: idx::Class) -> &Self::Output {
        &self.classes[idx]
    }
}
impl std::ops::IndexMut<idx::Class> for Ctx {
    fn index_mut(&mut self, idx: idx::Class) -> &mut Self::Output {
        &mut self.classes[idx]
    }
}

impl std::ops::Index<idx::Pack> for Ctx {
    type Output = Pack;
    fn index(&self, idx: idx::Pack) -> &Self::Output {
        &self.packs[idx]
    }
}
impl std::ops::IndexMut<idx::Pack> for Ctx {
    fn index_mut(&mut self, idx: idx::Pack) -> &mut Self::Output {
        &mut self.packs[idx]
    }
}

impl Ctx {
    /// Name of the root package.
    ///
    /// This package's name is not legal, on purpose, so that it does not clash with user-defined
    /// packages.
    pub const ROOT_PACKAGE_NAME: &str = "[root]";

    /// Name of the package containing builtin classes.
    ///
    /// This package's name is not legal, on purpose, so that it does not clash with user-defined
    /// packages.
    pub const BUILTIN_PACKAGE_NAME: &str = "[builtin]";

    /// Augments the context with builtin types.
    ///
    /// Called by [`Self::with_capacity`], the bottom-most constructor, no need to call this
    /// anywhere else.
    fn populate_builtin(&mut self) -> Res<()> {
        let path = Path::new(self.builtin_pack);
        let builtins = builtin::Typ::builders();
        for (typ, class_builder) in builtins {
            let c_idx = self.raw_add_class(|idx| class_builder(path.clone(), idx))?;
            let _prev = self.builtin_map.insert(typ, c_idx);
            if _prev.is_some() {
                panic!(
                    "[fatal] failed to populate builtin classes, trying to redefine `{:?}`",
                    typ,
                )
            }
        }
        Ok(())
    }

    /// Constructor.
    pub fn with_capacity(pack_capa: usize, class_capa: usize) -> Self {
        let mut packs = idx::PackMap::with_capacity(pack_capa);
        let top_pack = packs.push_idx(|idx| Pack::new(idx, "[root]", None));
        let builtin_pack =
            packs.push_idx(|idx| Pack::new(idx, Self::BUILTIN_PACKAGE_NAME, Some(top_pack)));
        packs[top_pack].add_sub(builtin_pack);
        let mut slf = Self {
            packs,
            top_pack,
            builtin_pack,
            name_to_pack: PathMap::new(),
            classes: idx::ClassMap::with_capacity(class_capa),
            name_to_class: PathMap::new(),
            builtin_map: HashMap::with_capacity(13),
            forward_ref_classes: BTreeSet::new(),
            forward_ref_packs: BTreeSet::new(),
        };
        slf.populate_builtin()
            .expect("[fatal] failed to populate builtin classes");
        slf
    }

    pub fn parse(txt: impl AsRef<str>) -> Res<Self> {
        let mut slf = Self::with_capacity(7, 7);
        parser::raw::Parser::parse(txt.as_ref(), &mut slf)?;
        slf.finalize()?;
        Ok(slf)
    }

    pub fn to_pretty_string(&self) -> String {
        let mut stack = vec![];
        let mut res = String::with_capacity(113);
        let mut current = self.top_pack;

        macro_rules! post {
            ($pref:expr, line $($interp_str:tt)*) => {{
                if !res.is_empty() {
                    res.push('\n');
                }
                res.push_str($pref);
                res.push_str(&format!($($interp_str)*));
            }};
        }

        'go_down: loop {
            let pref = &format!("{0:>1$}", "", stack.len() * 2);
            post!(pref, line "- {} #{}", self[current].name(), self[current].idx);

            // show classes
            let mut classes: Vec<idx::Class> = self[current].classes().iter().cloned().collect();
            classes.sort();
            for c_idx in classes {
                let class = &self[c_idx];
                post!(pref, line "  class {} [{}] #{}", class.name(), class.typ(), class.idx);

                if !class.sup().is_empty() {
                    let sups = class
                        .sup()
                        .iter()
                        .cloned()
                        .show_iter_cs(|idx| self[idx].name());
                    post!(pref, line "    supers: {}", sups);
                } else {
                    // post!(pref, line "    no super classes");
                }

                if !class.sub().is_empty() {
                    let subs = class
                        .sub()
                        .iter()
                        .cloned()
                        .show_iter_cs(|idx| self[idx].name());
                    post!(pref, line "    subers: {}", subs);
                } else {
                    // post!(pref, line "    no suber classes");
                }

                if !class.structural().is_empty() {
                    post!(pref, line "    structural features:");
                    for tural in class.structural() {
                        post!(pref, line "    - `{}`: {} of {} `{}`", tural.name, tural.kind, tural.bounds, self[tural.typ].name());
                    }
                }
            }

            // go down first sub if any
            let mut subs = {
                let mut subs: Vec<idx::Pack> = self[current].sub().iter().cloned().collect();
                subs.sort();
                subs.into_iter()
            };
            if let Some(next) = subs.next() {
                stack.push(subs);
                current = next;
                continue 'go_down;
            }

            // only reachable if there are no subpackages
            'go_up: loop {
                if let Some(mut subs) = stack.pop() {
                    if let Some(next) = subs.next() {
                        // new package to do gown into
                        stack.push(subs);
                        current = next;
                        continue 'go_down;
                    } else {
                        // no subs here, but stack might contain more
                        continue 'go_up;
                    }
                } else {
                    // stack is empty, we done
                    break 'go_down;
                }
            }
        }

        res.shrink_to_fit();
        res
    }

    pub fn top_pack(&self) -> idx::Pack {
        self.top_pack
    }
    pub fn builtin_pack(&self) -> idx::Pack {
        self.builtin_pack
    }

    pub fn pack_idx<K>(&self, path: &Path, name: &K) -> Res<idx::Pack>
    where
        String: Borrow<K>,
        K: std::hash::Hash + Eq + Display + ?Sized,
    {
        self.name_to_pack
            .unwrap_at(path, |p| p.display(&self.packs))
            .and_then(|map| {
                map.get(name)
                    .ok_or_else(|| error!(@unknown("package") name.to_string()))
                    .cloned()
            })
    }

    pub fn pack_idx_or_forward_ref(
        &mut self,
        path: &Path,
        name: impl AsRef<str>,
    ) -> Res<idx::Pack> {
        let name = name.as_ref();
        if let Ok(idx) = self.pack_idx(path, name) {
            Ok(idx)
        } else {
            let idx = self.add_pack(path.clone(), name)?;
            let _is_new = self.forward_ref_packs.insert(idx);
            debug_assert!(_is_new);
            Ok(idx)
        }
    }

    /// Map from [`builtin::Typ`] to [`idx::Class`].
    pub fn builtins(&self) -> &HashMap<builtin::Typ, idx::Class> {
        &self.builtin_map
    }
    /// Retrieves the [`idx::Class`] of a builtin class.
    pub fn get_builtin_idx(&self, typ: impl AsRef<builtin::Typ>) -> Res<idx::Class> {
        let typ = typ.as_ref();
        self.builtin_map.get(typ).cloned().ok_or_else(|| {
            error!(
                "[fatal] builtin type `{}` has not been properly registered",
                typ,
            )
        })
    }

    /// Parses a builtin type URL appearing after a `ecore:EDataType` in an `eType` attribute.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # ecore_rs::prelude! {}
    /// let ctx = Ctx::with_capacity(0, 0);
    /// let idx =
    ///     ctx.parse_builtin_etype_url("http://www.eclipse.org/emf/2002/Ecore#//EString")
    ///     .expect("url parsing failed");
    /// assert_eq!(ctx[idx].name(), "EString");
    /// assert_eq!(ctx[idx].path.last(), ctx.builtin_pack());
    /// ```
    pub fn parse_builtin_etype_url(&self, url: impl AsRef<str>) -> Res<idx::Class> {
        let typ = builtin::Typ::parse_etype_url(url)?;
        self.get_builtin_idx(typ)
    }
    /// Recognizes a builtin `ecore` datatype from its `eType` description.
    ///
    /// Returns `None` if `s` does not start with `ecore:EDataType`, otherwise returns the type if it is
    /// recognized from the URL part and an error if not.
    ///
    /// If you know you're parsing a builtin type, like if you already parsed the `ecore:EDataType`
    /// header of the `eType`, use [`Self::parse_builtin_etype_url`] instead.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # ecore_rs::prelude! {}
    /// let ctx = Ctx::with_capacity(0, 0);
    /// let idx =
    ///     ctx.try_parse_builtin_etype("ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString")
    ///     .expect("url parsing failed")
    ///     .expect("failed to parse `EString` builtin type");
    /// assert_eq!(ctx[idx].name(), "EString");
    /// assert_eq!(ctx[idx].path.last(), ctx.builtin_pack());
    ///
    /// let idx =
    ///     ctx.try_parse_builtin_etype("#//NotBuiltin")
    ///     .expect("non-builtin class parsing failed");
    /// assert_eq!(idx, None);
    ///
    /// let res =
    ///     ctx.try_parse_builtin_etype("ecore:EDataType http://www.some.non/sense");
    /// assert!(res.is_err());
    /// ```
    pub fn try_parse_builtin_etype(&self, etype: impl AsRef<str>) -> Res<Option<idx::Class>> {
        let typ_opt = builtin::Typ::try_parse_etype(etype)?;
        if let Some(typ) = typ_opt {
            self.get_builtin_idx(typ).map(Some)
        } else {
            Ok(None)
        }
    }

    pub fn class_names_at(&self, path: &Path) -> Res<&HashMap<String, idx::Class>> {
        self.name_to_class
            .unwrap_at(path, |p| p.display(&self.packs))
    }
    pub fn class_names_at_mut(&mut self, path: &Path) -> Res<&mut HashMap<String, idx::Class>> {
        self.name_to_class
            .unwrap_at_mut(path, |p| p.display(&self.packs))
    }
    pub fn class_names_at_mut_or_new(&mut self, path: Path) -> &mut HashMap<String, idx::Class> {
        self.name_to_class.at_mut_or_new(path)
    }

    pub fn pack_names_at(&self, path: &Path) -> Res<&HashMap<String, idx::Pack>> {
        self.name_to_pack
            .unwrap_at(path, |p| p.display(&self.packs))
    }
    pub fn pack_names_at_mut(&mut self, path: &Path) -> Res<&mut HashMap<String, idx::Pack>> {
        self.name_to_pack
            .unwrap_at_mut(path, |p| p.display(&self.packs))
    }
    pub fn pack_names_at_mut_or_new(&mut self, path: Path) -> &mut HashMap<String, idx::Pack> {
        self.name_to_pack.at_mut_or_new(path)
    }

    pub fn get_class_idx_in<K>(&self, path: &Path, name: &K) -> Res<idx::Class>
    where
        String: Borrow<K>,
        K: std::hash::Hash + Eq + Display + ?Sized,
    {
        self.class_names_at(path)
            .context(|| {
                format!(
                    "failed to resolve class `{}{}`",
                    path.display_sep(&self.packs),
                    name.to_string()
                )
            })?
            .get(name)
            .cloned()
            .ok_or_else(|| error!(@unknown("class") name.to_string()))
    }

    pub fn get_class_idx_or_forward_ref_in(
        &mut self,
        path: &Path,
        name: impl AsRef<str>,
    ) -> Res<idx::Class> {
        let name = name.as_ref();
        // do we already know this class at this path?
        if let Ok(idx) = self.get_class_idx_in(path, name) {
            return Ok(idx);
        }

        self.add_forward_ref_class(path.clone(), name)
    }

    pub fn add_pack(&mut self, path: Path, name: impl Into<String>) -> Res<idx::Pack> {
        let name = name.into();

        // do we know this package?
        if let Ok(p_idx) = self.pack_idx(&path, &name) {
            // either a definition for a previously forward-ref-ed package or a redef
            if self.forward_ref_packs.contains(&p_idx) {
                self.forward_ref_packs.remove(&p_idx);
                // the package should be empty at this point, since we're currently defining it
                if !self[p_idx].is_empty() {
                    bail!("failed to define previously forward-referenced package: not empty")
                }
                return Ok(p_idx);
            } else {
                bail!(@redef("package") name)
            }
        }

        let p_idx = self.packs.next_index();

        // retrieve local context, insert new if needed
        let local_pack_names = self.pack_names_at_mut_or_new(path.clone());
        // register pack index
        let _prev = local_pack_names.insert(name.clone(), p_idx);

        if let Some(p_idx) = _prev {
            let name = self[p_idx].name();
            let note = format!("existing package `{name}` has index `#{p_idx}`");
            return Err(error!(@redef("package") name).with_context(note));
        }

        let sup_idx = path.last();
        // register as sub-package in super
        let _is_new = self[sup_idx].add_sub(p_idx);
        // actually create sub-package
        let real_p_idx = self
            .packs
            .push_idx(|idx| Pack::new(idx, &name, Some(sup_idx)));
        debug_assert_eq!(p_idx, real_p_idx); // #defense
        Ok(real_p_idx)
    }

    fn raw_add_class(&mut self, build_class: impl FnOnce(idx::Class) -> Class) -> Res<idx::Class> {
        // register class
        let c_idx = self.classes.push_idx(build_class);
        let parent = self[c_idx].path.last();
        // update relevant package
        let _is_new = self[parent].classes_insert(c_idx);
        if !_is_new {
            bail!(
                "class `{}` is already in package `{}`",
                self[c_idx].name(),
                self[parent].name()
            );
        }
        Ok(c_idx)
    }

    /// Adds a class to the context.
    ///
    /// If `path`/`name` is already registered as forward-referenced, the dummy class is replaced by
    /// the actual definition and removed from `self.forward_refs`.
    pub fn add_class(
        &mut self,
        path: Path,
        typ: impl Into<String>,
        name: impl Into<String>,
        inst_name: Option<impl Into<String>>,
        is_abstract: Option<bool>,
        is_interface: Option<bool>,
    ) -> Res<idx::Class> {
        let name = name.into();

        macro_rules! build_class {
            ($idx:expr) => {
                Class::new($idx, path, typ, name, inst_name, is_abstract, is_interface)
            };
        }

        // do we know have a `name` at this `path`?
        if let Some(idx) = self
            .name_to_class
            .get(&path)
            .and_then(|name_map| name_map.get(&name))
            .cloned()
        {
            // either a class redef', or it's a forward ref and we need to handle it
            if self.forward_ref_classes.contains(&idx) {
                self.forward_ref_classes.remove(&idx);
                let class = build_class!(idx);
                let _dummy = std::mem::replace(&mut self[idx], class);
                // don't need to update the class' parent, the forward ref already did that
                return Ok(idx);
            } else {
                return Err(error!(@redef("class") name)
                    .with_context(format!("in package `{}`", path.display(&self.packs))));
            }
        }

        // actually new `path`/`name` class
        let c_idx = self.classes.next_index();

        // retrieve local context, insert new if needed
        let local_class_names = self.class_names_at_mut_or_new(path.clone());
        // register class index
        let _prev = local_class_names.insert(name.clone(), c_idx);

        // check for name clashes
        if let Some(c_idx) = _prev {
            let name = self[c_idx].name();
            let note = format!("existing class `{name}` has index `#{c_idx}`");
            return Err(error!(@redef("class") name).with_context(note));
        }

        let real_c_idx = self.raw_add_class(|c_idx| {
            Class::new(
                c_idx,
                path.clone(),
                typ,
                name,
                inst_name,
                is_abstract,
                is_interface,
            )
        })?;
        debug_assert_eq!(c_idx, real_c_idx); // #defense

        Ok(real_c_idx)
    }

    /// Adds a dummy class and registers it as a forward reference.
    ///
    /// Also registers the class as a member of its parent package.
    pub fn add_forward_ref_class(
        &mut self,
        path: Path,
        name: impl Into<String>,
    ) -> Res<idx::Class> {
        let msg = "[forward referenced class placeholder]";
        let c_idx = self.add_class(path, msg, name, None as Option<String>, None, None)?;
        let _is_new = self.forward_ref_classes.insert(c_idx);
        debug_assert!(_is_new);
        Ok(c_idx)
    }

    /// Registers `sup` as a super-class of `sub`.
    pub fn add_sup_class(&mut self, sup: idx::Class, sub: idx::Class) {
        let _is_new = self[sup].add_sub(sub);
        let _is_new = self[sub].add_sup(sup);
    }

    /// Packs appear in the order they were added in.
    pub fn packs(&self) -> &idx::PackMap<Pack> {
        &self.packs
    }
    pub fn pack_indices<'me>(&'me self) -> impl Iterator<Item = idx::Pack> + 'me {
        self.packs.indices()
    }

    /// Classes appear in the order they were added in.
    pub fn classes(&self) -> &[Class] {
        &self.classes
    }
    pub fn class_indices<'me>(&'me self) -> impl Iterator<Item = idx::Class> + 'me {
        self.classes.indices()
    }
    pub fn abstract_classes(&self) -> impl Iterator<Item = &Class> {
        self.classes.iter().filter(|c| c.is_abstract())
    }
    pub fn concrete_classes(&self) -> impl Iterator<Item = &Class> {
        self.classes.iter().filter(|c| c.is_concrete())
    }

    /// Classes appearing in a given package.
    pub fn classes_in<'a>(&'a self, path: &'a Path) -> impl Iterator<Item = &Class> + 'a {
        self.classes.iter().filter(move |c| &c.path == path)
    }
    pub fn abstract_classes_in<'a>(&'a self, path: &'a Path) -> impl Iterator<Item = &Class> + 'a {
        self.classes_in(path).filter(|c| c.is_abstract())
    }
    pub fn concrete_classes_in<'a>(&'a self, path: &'a Path) -> impl Iterator<Item = &Class> + 'a {
        self.classes_in(path).filter(|c| c.is_concrete())
    }

    pub fn enter_pack(&mut self, path: Path) -> Res<PathCtx> {
        Ok(PathCtx { path, ctx: self })
    }

    pub fn enter_root_pack(&mut self) -> Res<PathCtx> {
        self.enter_pack(Path::new(self.top_pack))
    }

    /// Finalizes the context.
    ///
    /// - checks there are no forward-referenced classes left;
    /// - checks there are no forward-referenced packages left;
    pub fn finalize(&mut self) -> Res<()> {
        let mut errors = false;

        if !self.forward_ref_classes.is_empty() {
            errors = true;
            log::error!("some forward-referenced classes have not been defined:");
            for idx in self.forward_ref_classes.iter().cloned() {
                log::error!("- `{}`", self[idx].display(self));
            }
        }

        if !self.forward_ref_packs.is_empty() {
            errors = true;
            log::error!("some forward-referenced classes have not been defined:");
            for idx in self.forward_ref_packs.iter().cloned() {
                log::error!("- `{}`", self[idx].path(self).display(self.packs()));
            }
        }

        if errors {
            bail!("failed to finalize parsing context")
        } else {
            Ok(())
        }
    }
}

pub struct PathCtx<'a> {
    path: Path,
    ctx: &'a mut Ctx,
}

impl<'a> std::ops::Index<idx::Class> for PathCtx<'a> {
    type Output = Class;
    fn index(&self, idx: idx::Class) -> &Self::Output {
        &self.ctx[idx]
    }
}
impl<'a> std::ops::IndexMut<idx::Class> for PathCtx<'a> {
    fn index_mut(&mut self, idx: idx::Class) -> &mut Self::Output {
        &mut self.ctx[idx]
    }
}

// impl<'a> std::ops::Index<idx::Pack> for PathCtx<'a> {
//     type Output = Pack;
//     fn index(&self, idx: idx::Pack) -> &Self::Output {
//         &self.ctx[idx]
//     }
// }
// impl<'a> std::ops::IndexMut<idx::Pack> for PathCtx<'a> {
//     fn index_mut(&mut self, idx: idx::Pack) -> &mut Self::Output {
//         &mut self.ctx[idx]
//     }
// }

impl<'a> PathCtx<'a> {
    pub fn current(&self) -> &Pack {
        &self.ctx[self.path.last()]
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
    pub fn display_path(&self) -> String {
        self.path.display(&self.ctx.packs)
    }
    pub fn display_path_sep(&self) -> String {
        self.path.display_sep(&self.ctx.packs)
    }
    pub fn current_pack(&self) -> &Pack {
        &self.ctx[self.path.last()]
    }

    pub fn ctx(&self) -> &Ctx {
        self.ctx
    }
    pub fn ctx_mut(&mut self) -> &mut Ctx {
        self.ctx
    }

    pub fn resolve_etype(&mut self, s: impl AsRef<str>) -> Res<idx::Class> {
        repr::Path::resolve_etype(self, s)
    }

    pub fn add_sub_pack(&mut self, name: impl Into<String>) -> Res<idx::Pack> {
        self.ctx.add_pack(self.path.clone(), name)
    }
    /// Same as [`Self::add_sub_pack`] but makes the context enter the new sub-package.
    pub fn add_and_enter_sub_pack_mut(&mut self, name: impl Into<String>) -> Res<idx::Pack> {
        let p_idx = self.ctx.add_pack(self.path.clone(), name)?;
        self.path.push(p_idx);
        Ok(p_idx)
    }
    /// Same as [`Self::add_sub_pack`] but makes the context enter the new sub-package.
    pub fn add_and_enter_sub_pack(mut self, name: impl Into<String>) -> Res<(idx::Pack, Self)> {
        let p_idx = self.add_and_enter_sub_pack_mut(name)?;
        Ok((p_idx, self))
    }

    pub fn add_class(
        &mut self,
        typ: impl Into<String>,
        name: impl Into<String>,
        inst_name: Option<impl Into<String>>,
        is_abstract: Option<bool>,
        is_interface: Option<bool>,
    ) -> Res<idx::Class> {
        self.ctx.add_class(
            self.path.clone(),
            typ,
            name,
            inst_name,
            is_abstract,
            is_interface,
        )
    }

    pub fn class_idx<K>(&self, name: &K) -> Res<idx::Class>
    where
        String: Borrow<K>,
        K: std::hash::Hash + Eq + Display + ?Sized,
    {
        self.ctx.get_class_idx_in(&self.path, name)
    }
    pub fn forward_ref_or_class_idx<K>(&mut self, name: impl AsRef<str>) -> Res<idx::Class> {
        self.ctx.get_class_idx_or_forward_ref_in(&self.path, name)
    }

    pub fn class_idx_in<K>(&self, path: &Path, name: &K) -> Res<idx::Class>
    where
        String: Borrow<K>,
        K: std::hash::Hash + Eq + Display + ?Sized,
    {
        self.ctx.get_class_idx_in(path, name)
    }
    pub fn forward_ref_or_class_idx_in(
        &mut self,
        path: &Path,
        name: impl AsRef<str>,
    ) -> Res<idx::Class> {
        self.ctx.get_class_idx_or_forward_ref_in(path, name)
    }

    /// Classes appear in the order they were added in.
    pub fn classes(&self) -> impl Iterator<Item = &Class> {
        self.ctx.classes_in(&self.path)
    }
    pub fn abstract_classes(&self) -> impl Iterator<Item = &Class> {
        self.ctx.abstract_classes_in(&self.path)
    }
    pub fn concrete_classes(&self) -> impl Iterator<Item = &Class> {
        self.ctx.concrete_classes_in(&self.path)
    }

    /// Adds an annotation to the current package.
    pub fn add_annotation(&mut self, annot: repr::Annot) {
        self.ctx[self.path.last()].add_annotation(annot)
    }

    /// Enters a sub-package.
    ///
    /// Error if `sub` is not a sub-package of the current package.
    pub fn enter_sub_pack_mut(&mut self, sub: idx::Pack) -> Res<()> {
        if !self.current_pack().has_sub(sub) {
            let path = self.path().display(&self.ctx.packs);
            let sub = self.ctx[sub].name();
            bail!("package `{path}` has no sub-package called `{sub}`")
        }
        self.path.push(sub);
        Ok(())
    }

    /// Enters a sub-package by consuming itself.
    pub fn enter_sub_pack(mut self, sub: idx::Pack) -> Res<Self> {
        self.enter_sub_pack_mut(sub)?;
        Ok(self)
    }

    /// Enters the super-package of the current package.
    ///
    /// Error if at top-level.
    pub fn enter_sup_pack(&mut self) -> Res<idx::Pack> {
        if let Some(popped) = self.path.pop() {
            Ok(popped)
        } else {
            bail!("trying to enter super package of top-level package")
        }
    }

    /// Enters a different package specified with an absolute path.
    pub fn change_pack(&mut self, path: Path) {
        self.path = path;
    }

    /// Registers `sup` as a super-class of `sub`.
    pub fn add_sup_class(&mut self, sup: idx::Class, sub: idx::Class) {
        self.ctx.add_sup_class(sup, sub)
    }

    pub fn enter_class<'me>(
        &'me mut self,
        typ: impl Into<String>,
        name: impl Into<String>,
        inst_name: Option<impl Into<String>>,
        is_abstract: Option<bool>,
        is_interface: Option<bool>,
    ) -> Res<ClassCtx<'a, 'me>> {
        let c_idx = self.ctx.add_class(
            self.path.clone(),
            typ,
            name,
            inst_name,
            is_abstract,
            is_interface,
        )?;
        Ok(ClassCtx { ctx: self, c_idx })
    }
}

pub struct ClassCtx<'a, 'b> {
    ctx: &'b mut PathCtx<'a>,
    c_idx: idx::Class,
}

impl<'a, 'b> std::ops::Index<idx::Class> for ClassCtx<'a, 'b> {
    type Output = Class;
    fn index(&self, idx: idx::Class) -> &Self::Output {
        &self.ctx[idx]
    }
}
impl<'a, 'b> std::ops::IndexMut<idx::Class> for ClassCtx<'a, 'b> {
    fn index_mut(&mut self, idx: idx::Class) -> &mut Self::Output {
        &mut self.ctx[idx]
    }
}

impl<'a, 'b> ClassCtx<'a, 'b> {
    pub fn current(&self) -> &repr::Class {
        &self.ctx[self.c_idx]
    }

    pub fn path_ctx(&self) -> &PathCtx<'a> {
        self.ctx
    }
    pub fn path_ctx_mut(&mut self) -> &mut PathCtx<'a> {
        self.ctx
    }

    pub fn ctx(&self) -> &Ctx {
        self.path_ctx().ctx()
    }
    pub fn ctx_mut(&mut self) -> &mut Ctx {
        self.ctx.ctx
    }

    pub fn resolve_etype(&mut self, s: impl AsRef<str>) -> Res<idx::Class> {
        repr::Path::resolve_etype(self.ctx, s)
    }

    pub fn add_annotation(&mut self, annot: repr::Annot) {
        self.ctx[self.c_idx].add_annotation(annot)
    }
    pub fn add_sup_class(&mut self, sup: idx::Class) {
        self.ctx.add_sup_class(sup, self.c_idx)
    }
    pub fn add_literal(&mut self, lit: repr::ELit) {
        self.ctx[self.c_idx].add_literal(lit)
    }
    pub fn add_operation(&mut self, op: repr::Operation) {
        self.ctx[self.c_idx].add_operation(op)
    }
    pub fn add_attribute(&mut self) {
        // println!("registering attribute")
    }
    pub fn add_structural(&mut self, s: repr::Structural) {
        self.ctx[self.c_idx].add_structural(s)
    }
    pub fn finalize(self) {
        // // not shrink-to-fit-ing since some stuff is postponed
        // self.ctx[self.c_idx].shrink_to_fit()
    }
}
