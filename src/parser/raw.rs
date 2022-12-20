prelude! {
    ctx::*,
    regex::Regex,
}

pub mod helpers {
    use super::*;

    pub fn bool(s: impl AsRef<str>) -> Res<bool> {
        match s.as_ref() {
            "true" | "True" => Ok(true),
            "false" | "False" => Ok(false),
            s => bail!("expected boolean, got `{}`", s),
        }
    }
}

pub struct Parser<'input> {
    txt: &'input str,
    cursor: usize,
}

/// # Parsing entry point
impl<'input> Parser<'input> {
    pub fn parse(txt: &'input str, ctx: &mut Ctx) -> Res<()> {
        let mut slf = Self::new(txt);
        match slf.top(ctx) {
            Ok(res) => Ok(res),
            Err(mut err) => {
                if !slf.at_eoi() {
                    err = err.context(|| {
                        let mut blah = "current parser state:".to_string();
                        for line in slf.tail().lines().take(2) {
                            blah.push_str("\n| `");
                            blah.push_str(line);
                            blah.push('`');
                        }
                        blah
                    })
                }
                return Err(err.with_context("parsing failed"));
            }
        }
    }

    fn top(&mut self, ctx: &mut Ctx) -> Res<()> {
        // ignore xml header
        let _ = self.until_char('<', true);
        let _ = self.until_char('>', true);

        let mut path = ctx.enter_root_pack()?;
        self.at_path(&mut path)
    }
}

/// ## Constructors
impl<'input> Parser<'input> {
    pub fn new(txt: &'input str) -> Self {
        Self { txt, cursor: 0 }
    }
}

/// ## Basic parsing-related features
impl<'input> Parser<'input> {
    pub fn at_eoi(&self) -> bool {
        self.cursor >= self.txt.len()
    }

    pub fn fail_on_eoi(&self) -> Res<()> {
        if self.at_eoi() {
            bail!("reached end of input unexpectedly")
        }
        Ok(())
    }

    pub fn handle_redef<From, Into>(
        &self,
        desc: impl Display,
        name: impl Display,
        from: Option<&From>,
        into: Into,
    ) -> Res<()>
    where
        From: Display,
        Into: Display,
    {
        if let Some(from) = from {
            log::warn!(
                "XML-level {} redefinition for `{}` from `{}` to `{}`",
                desc,
                name,
                from,
                into
            )
        }
        Ok(())
    }

    pub fn tail(&self) -> &'input str {
        &self.txt[self.cursor..]
    }

    pub fn debug_show_tail_n(&self, n: usize, pref: impl AsRef<str>) {
        let pref = pref.as_ref();
        for line in self.tail().lines().take(n) {
            log::debug!("{}`{}`", pref, line)
        }
    }
}

/// ## Basic parsers
impl<'input> Parser<'input> {
    pub fn ws(&mut self) {
        for c in self.tail().chars() {
            if c.is_whitespace() {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    pub fn raw_tag(&mut self, tag: impl AsRef<str>) -> Res<()> {
        let tag = tag.as_ref();
        let okay = self.try_raw_tag(tag);
        if !okay {
            log::error!("|==[raw_tag] failure");
            log::error!("| parser tail (next two lines):");
            for line in self.tail().lines().take(2) {
                log::error!("| `{}`", line)
            }
            log::error!("|==|");
            bail!("expected tag `{}`", tag)
        }
        Ok(())
    }
    pub fn try_raw_tag(&mut self, tag: impl AsRef<str>) -> bool {
        let tag = tag.as_ref();
        let tail = self.tail();
        if tag.len() < self.tail().len() {
            if tail.starts_with(tag) {
                self.cursor += tag.len();
                return true;
            }
        }
        false
    }

    pub fn tag(&mut self, tag: impl AsRef<str>) -> Res<()> {
        let tag = tag.as_ref();
        if !self.try_tag(tag) {
            log::error!("|==[tag] failure");
            log::error!("| parser tail:");
            for line in self.tail().lines().take(2) {
                log::error!("| `{}`", line)
            }
            log::error!("|==|");
            bail!("expected tag `{}`", tag)
        }
        Ok(())
    }
    pub fn try_tag(&mut self, tag: impl AsRef<str>) -> bool {
        let mem = self.cursor;
        let success = self.try_raw_tag(tag);
        if success {
            if let Some(c) = self.tail().chars().next() {
                if c.is_ascii_alphanumeric() {
                    self.cursor = mem;
                    return false;
                }
            }
        }
        success
    }

    pub fn regex(&mut self, re: Regex) -> &'input str {
        self.try_regex(re).unwrap()
    }
    pub fn try_regex(&mut self, re: Regex) -> Option<&'input str> {
        if let Some(m4tch) = re.find(self.tail()) {
            debug_assert_eq!(m4tch.start(), 0);
            let res = m4tch.as_str();
            self.cursor += res.len();
            Some(res)
        } else {
            None
        }
    }

    pub fn ident(&mut self) -> Res<&'input str> {
        if let Some(id) = self.try_ident() {
            Ok(id)
        } else {
            bail!("expected identifier")
        }
    }
    pub fn try_ident(&mut self) -> Option<&'input str> {
        self.try_regex(Regex::new(r#"^[a-zA-Z_][a-zA-Z_\-0-9]*"#).unwrap())
    }

    pub fn colon_ident(&mut self) -> &'input str {
        self.try_colon_ident().unwrap()
    }
    pub fn try_colon_ident(&mut self) -> Option<&'input str> {
        self.try_regex(Regex::new(r#"^[a-zA-Z_][:a-zA-Z_\-0-9]*"#).unwrap())
    }

    pub fn until_char(&mut self, c: char, inclusive: bool) -> &'input str {
        self.until_char_is(|char| char == c, inclusive)
    }
    pub fn until_char_is(
        &mut self,
        stop_at: impl Fn(char) -> bool,
        inclusive: bool,
    ) -> &'input str {
        let start = self.cursor;
        for char in self.tail().chars() {
            if stop_at(char) {
                if inclusive {
                    self.cursor += 1;
                }
                break;
            } else {
                self.cursor += 1;
            }
        }
        &self.txt[start..self.cursor]
    }

    pub fn dquote_str(&mut self) -> Res<&'input str> {
        // log::trace!(
        //     "[dquote_str] tail: `{}`",
        //     self.tail().lines().next().unwrap()
        // );
        self.raw_tag("\"")?;
        let inner = self.until_char('"', false);
        self.raw_tag("\"")?;
        Ok(inner)
    }
}

/// ## Low-level XML parsers
impl<'input> Parser<'input> {
    /// Parses an XML attribute of the form `<ident>\s*=\s*<double-quoted-string>`.
    pub fn xml_ident_attribute(&mut self) -> Res<(&'input str, &'input str)> {
        let ident = self.ident()?;
        self.ws();
        self.raw_tag("=")?;
        let val = self
            .dquote_str()
            .context(|| format!("parsing a value for xml attribute `{}`", ident))?;
        Ok((ident, val))
    }

    /// Parses an XML attribute of the form `<ident>\s*=\s*<double-quoted-string>`.
    pub fn xml_colon_ident_attribute(
        &mut self,
    ) -> Res<(SmallVec<[&'input str; 4]>, &'input str, &'input str)> {
        let mut pref = smallvec![];
        let mut ident = self.ident()?;
        'until_equal_sign: loop {
            self.ws();
            if self.try_raw_tag("=") {
                break 'until_equal_sign;
            }
            self.raw_tag(":")?;
            pref.push(ident);
            ident = self.ident()?
        }
        self.ws();
        let val = self
            .dquote_str()
            .context(|| format!("parsing a value for xml attribute `{}`", ident))?;
        Ok((pref, ident, val))
    }

    /// Parses an XML attribute of the form `<ident>\s*=\s*<double-quoted-string>`.
    pub fn named_xml_attribute(&mut self, name: impl AsRef<str>) -> Res<&'input str> {
        let name = name.as_ref();
        self.tag(name)?;
        self.ws();
        self.raw_tag("=")?;
        let val = self
            .dquote_str()
            .context(|| format!("parsing a value for xml attribute `{}`", name))?;
        Ok(val)
    }
}

/// Helper macro to streamline tedious parsing stuff.
macro_rules! parse {
    (
        $slf:ident . attributes for $desc:literal until($stop:expr) $(
            $name:ident = $( [$($key_pref:ident),* $(,)?] )? $key:ident
                $(.map |$val:ident| $map:expr,)?
                $(.ok_or_else $err:expr,)?
        )+
    ) => {
        $( let mut $name = None; )*

        'work: loop {
            if $stop {
                break 'work;
            }

            let (key_pref, key, val) = $slf.xml_colon_ident_attribute()?;
            $slf.ws();
            match (&*key_pref, key) {
                $(
                    (
                        [
                            $($(stringify!($key_pref),)*)?
                        ],
                        stringify!($key)
                    ) => {
                        $slf.handle_redef(
                            concat!($desc, " attribute"),
                            stringify!($name),
                            $name.as_ref(),
                            val
                        )?;
                        $(
                            let val = {
                                let $val = val;
                                $map
                            };
                        )?
                        $name = Some(val);
                    }
                )+
                _ => {
                    let mut key = key.to_string();
                    for pref in key_pref.iter().rev() {
                        key = format!("{pref}:{key}");
                    }
                    let mut help = format!("expected one of");
                    $(
                        help.push_str(
                            concat!(
                                " `",
                                $($(stringify!($key_pref), ":",)*)?
                                stringify!($key)
                                , "`"
                            )
                        );
                    )*
                    return Err(
                        error!(@unexpected(concat!($desc, " attribute")) key)
                            .with_context(help)
                    );
                }
            }
        }

        $(
            let $name = $name
                $( .ok_or_else(|| $err)? )?
            ;
        )*

    }
}

/// ## Package parsing
impl<'input> Parser<'input> {
    pub fn at_path(&mut self, ctx: &mut PathCtx) -> Res<()> {
        'walk: loop {
            self.ws();
            if self.try_tag("<ecore:EPackage") {
                self.ws();
                self.enter_package(ctx)?;
                continue 'walk;
            } else if self.try_tag("<eClassifiers") {
                self.class(ctx)?;
                continue 'walk;
            }

            if self.at_eoi() {
                break 'walk Ok(());
            }
            // not a package, not a class, not done, only legal thing is package closer goes up the
            // current package
            self.package_closer(ctx)?;
            self.ws();
        }
    }

    pub fn enter_package(&mut self, ctx: &mut PathCtx) -> Res<()> {
        parse! {
            self.attributes for "package" until( self.try_raw_tag(">") )
                name = name
                    .map |s| s.to_string(),
                    .ok_or_else error!(@unexpected("package") "with no name"),
                _version = [xmi] version
                _xmi = [xmlns] xmi
                _xsi = [xmlns] xsi
                _type = [xmlns] ecore
        }
        ctx.add_and_enter_sub_pack_mut(name)?;
        Ok(())
    }

    pub fn package_closer(&mut self, ctx: &mut PathCtx) -> Res<()> {
        if self.try_raw_tag("</ecore:EPackage>") {
            ctx.enter_sup_pack()?;
            Ok(())
        } else {
            log::error!("|==[package_closer] failure");
            log::error!("| tail:");
            for line in self.tail().lines().take(2) {
                log::error!("| `{line}`")
            }
            log::error!("|==|");
            bail!("expected package, type, or package closer")
        }
    }
}

/// ## Class parsing
impl<'input> Parser<'input> {
    pub fn class(&mut self, ctx: &mut PathCtx) -> Res<()> {
        let (mut typ, mut name, mut inst_name, mut is_abstract, mut is_interface, mut sup_typs) =
            (None, None, None, None, None, None);
        // operations XML tags can be closed directly with `/>`, or have parameters and end with
        // `</eOperations>`; this flag indicates the former
        let mut early_done = false;

        'attributes: loop {
            self.ws();
            if self.try_raw_tag(">") {
                break 'attributes;
            } else if self.try_raw_tag("/>") {
                early_done = true;
                break 'attributes;
            }

            let (key_pref, key, val) = self.xml_colon_ident_attribute()?;

            match (&*key_pref, key) {
                ([], "name") => {
                    self.handle_redef("class attribute", "name", name.as_ref(), val)?;
                    name = Some(val)
                }
                ([], "instanceTypeName") => {
                    self.handle_redef(
                        "class attribute",
                        "instanceTypeName",
                        inst_name.as_ref(),
                        val,
                    )?;
                    inst_name = Some(val)
                }
                ([], "interface") => {
                    // TODO: factor bool/int/... value parsing outta here
                    let is_int = match val {
                        "true" => true,
                        "false" => false,
                        _ => {
                            return Err(error!(@unexpected("boolean value") val).with_context(
                                format!("failed to parse value of class attribute `{key}`"),
                            ))
                        }
                    };
                    self.handle_redef("class attribute", "interface", is_interface.as_ref(), key)?;
                    is_interface = Some(is_int)
                }
                ([], "abstract") => {
                    // TODO: factor bool/int/... value parsing outta here
                    let is_abs = match val {
                        "true" => true,
                        "false" => false,
                        _ => {
                            return Err(error!(@unexpected("boolean value") val).with_context(
                                format!("failed to parse value of class attribute `{key}`"),
                            ))
                        }
                    };
                    self.handle_redef("class attribute", "abstract", is_abstract.as_ref(), key)?;
                    is_abstract = Some(is_abs)
                }
                ([], "eSuperTypes") => {
                    if let Some(sup_typs) = sup_typs.as_ref() {
                        log::warn!(
                            "XML-level class attribute redefinition for `eSuperTypes` from `{}` to `{}`",
                            sup_typs,
                            val,
                        );
                    }
                    sup_typs = Some(val);
                }
                (["xsi"], "type") => {
                    if let Some(typ) = typ.as_ref() {
                        log::warn!(
                            "XML-level class attribute redefinition for `xsi:type` from `{}` to `{}`",
                            typ,
                            val,
                        );
                    }
                    typ = Some(val);
                }
                _ => {
                    let mut att = key.to_string();
                    for pref in key_pref.iter().rev() {
                        att = format!("{pref}:{att}");
                    }
                    bail!(@unexpected("`eClassifiers` attribute") att)
                }
            }
        }

        let name = name.ok_or_else(|| error!(@unexpected("`eClassifier`") "with no name"))?;
        let typ =
            typ.ok_or_else(|| error!("`eClassifier` named `{}` does not specify its type", name))?;

        #[allow(unused_mut)]
        let mut class_ctx = ctx.enter_class(typ, name, inst_name, is_abstract, is_interface)?;

        if !early_done {
            // log::debug!("parsing content of class `{}`", class_ctx.current().name());
            self.class_content(&mut class_ctx)
                .context(|| format!("failed to parse class `{}`", class_ctx.current().name()))?;
        }

        if let Some(sup_typs) = sup_typs {
            for sup_typ in sup_typs.split(" ").filter_map(|bit| {
                let bit = bit.trim();
                if bit.is_empty() {
                    None
                } else {
                    Some(bit)
                }
            }) {
                let sup_idx = class_ctx.resolve_etype(sup_typ)?;
                class_ctx.add_sup_class(sup_idx);
            }
        }

        class_ctx.finalize();

        Ok(())
    }

    pub fn class_content(&mut self, ctx: &mut ClassCtx) -> Res<()> {
        'content: loop {
            self.ws();

            if self.try_raw_tag("</eClassifiers>") {
                // log::debug!("parsing class end for `{}`", ctx.current().name());
                break 'content;
            }

            if self.try_tag("<eAnnotations") {
                self.ws();
                let annot = self
                    .annotation()
                    .with_context("failed to parse class annotation")?;
                ctx.add_annotation(annot);
                continue 'content;
            } else if self.try_tag("<eOperations") {
                self.ws();
                let op = self
                    .class_operation(ctx)
                    .with_context("failed to parse class operation")?;
                ctx.add_operation(op);
            } else if self.try_tag("<eLiterals") {
                self.ws();
                let lit = self
                    .class_literal()
                    .with_context("failed to parse class literal")?;
                ctx.add_literal(lit);
            } else if self.try_tag("<eStructuralFeatures") {
                self.ws();
                let structural = self
                    .class_structural(ctx)
                    .with_context("failed to parse structural features")?;
                // log::info!("done parsing structural");
                ctx.add_structural(structural);
            } else {
                bail!("unexpected class content")
            }
        }
        Ok(())
    }

    /// Parses everything **after** a `<eAnnotations` until a `</eAnnotations>`.
    ///
    /// Expects no leading whitespaces, as all parsers do except for the top-level one.
    pub fn annotation(&mut self) -> Res<repr::Annot> {
        let source = self.named_xml_attribute("source")?;
        self.ws();
        self.tag(">")?;
        let mut annot = repr::Annot::with_capacity(source, 3);

        // log::debug!("|==| post source:");
        // self.debug_show_tail_n(2, "| ");

        // parse `<details
        'details: loop {
            self.ws();

            // done with this annotation?
            if self.try_tag("</eAnnotations>") {
                break 'details;
            }

            // parse `<details key="..." value="..."/>`
            {
                self.tag("<details")?;
                self.ws();

                let (att1, val1) = self.xml_ident_attribute()?;
                match att1 {
                    "key" => {
                        self.ws();
                        let (att2, val2) = self.xml_ident_attribute()?;
                        match att2 {
                            "value" => annot.insert(val1, val2)?,
                            _ => bail!("expected `value` attribute, found `{}`", att2),
                        }
                    }
                    _ => bail!("expected `key` attribute, found `{}`", att1),
                }

                self.ws();
                self.tag("/>")?
            }
        }

        annot.shrink_to_fit();
        Ok(annot)
    }

    pub fn class_literal(&mut self) -> Res<repr::ELit> {
        let (mut name, mut value): (Option<&str>, Option<&str>) = (None, None);

        'inner: loop {
            if self.try_raw_tag("/>") {
                break 'inner;
            }
            let (key, val) = self.xml_ident_attribute()?;
            match key {
                "name" => {
                    self.handle_redef("class literal", "name", name.as_ref(), val)?;
                    name = Some(val);
                }
                "value" => {
                    self.handle_redef("class literal", "value", value.as_ref(), val)?;
                    value = Some(val);
                }
                _ => bail!(@unexpected("class literal attribute") key),
            }
            self.ws();
        }

        if let Some(name) = name {
            Ok(repr::ELit::new(name, value))
        } else {
            bail!(@unexpected("class literal") "with no name")
        }
    }

    pub fn class_parameter(&mut self, ctx: &mut ClassCtx) -> Res<repr::Param> {
        let (mut name, mut lbound, mut ubound, mut typ) = (None, None, None, None);
        '_attributes: while !self.try_raw_tag("/>") {
            let (key, val) = self.xml_ident_attribute()?;
            match key {
                "name" => {
                    self.handle_redef("parameter name", "name", name.as_ref(), val)?;
                    name = Some(val);
                }
                "lowerBound" => {
                    self.handle_redef("parameter lower bound", "lowerBound", lbound.as_ref(), val)?;
                    lbound = Some(val);
                }
                "upperBound" => {
                    self.handle_redef("parameter upper bound", "upperBound", ubound.as_ref(), val)?;
                    ubound = Some(val);
                }
                "eType" => {
                    self.handle_redef("parameter type", "eType", typ.as_ref(), val)?;
                    typ = Some(val);
                }
                _ => {
                    bail!(@unexpected("parameter attribute key") key)
                }
            }
            self.ws();
        }

        let name = name.ok_or_else(|| error!(@unexpected("class parameter") "with no name"))?;
        let typ = {
            let etyp = typ.ok_or_else(
                || error!(@unexpected(format!("class parameter `{name}`")) "with no type"),
            )?;
            ctx.resolve_etype(etyp)?
        };
        let bounds = repr::Bounds::from_str(lbound, ubound)
            .context(|| format!("illegal bounds for parameter `{name}`"))?;

        Ok(repr::Param::new(name, bounds, typ))
    }

    pub fn class_operation(&mut self, ctx: &mut ClassCtx) -> Res<repr::Operation> {
        let (mut name, mut typ) = (None, None);
        // operations XML tags can be closed directly with `/>`, or have parameters and end with
        // `</eOperations>`; this flag indicates the former
        let mut early_done = false;

        'attributes: loop {
            let (key, val) = self.xml_ident_attribute()?;
            match key {
                "name" => {
                    self.handle_redef("operation name", "name", name.as_ref(), val)?;
                    name = Some(val);
                }
                "eType" => {
                    self.handle_redef("operation type", "eType", typ.as_ref(), val)?;
                    typ = Some(val);
                }
                _ => bail!(@unexpected("operation attribute") key),
            }

            self.ws();

            if self.try_raw_tag("/>") {
                early_done = true;
                break 'attributes;
            } else if self.try_raw_tag(">") {
                break 'attributes;
            }
        }

        let name = name.ok_or_else(|| error!(@unexpected("`eOperation`") "with no name"))?;

        let typ = if let Some(typ) = typ {
            Some(ctx.resolve_etype(typ)?)
        } else {
            None
        };

        let mut operation = repr::Operation::new(name, typ);

        if !early_done {
            self.ws();

            while !self.try_raw_tag("</eOperations>") {
                self.tag("<eParameters")?;
                self.ws();
                let param = self.class_parameter(ctx)?;
                operation.add_parameter(param);
                self.ws();
            }
        }

        Ok(operation)
    }

    pub fn class_structural(&mut self, ctx: &mut ClassCtx) -> Res<repr::Structural> {
        let (
            mut typ,
            mut name,
            mut lbound,
            mut ubound,
            mut etype,
            mut opposite,
            mut containment,
            mut is_id,
        ) = (None, None, None, None, None, None, None, None);

        'attributes: loop {
            let (pref, key, val) = self.xml_colon_ident_attribute()?;
            match (&*pref, key) {
                ([], "name") => {
                    self.handle_redef("class", "name", name.as_ref(), val)?;
                    name = Some(val);
                }
                (["xsi"], "type") => {
                    self.handle_redef("class", "xsi:type", typ.as_ref(), val)?;
                    typ = Some(val);
                }
                ([], "lowerBound") => {
                    self.handle_redef("class", "lowerBound", lbound.as_ref(), val)?;
                    lbound = Some(val);
                }
                ([], "upperBound") => {
                    self.handle_redef("class", "upperBound", ubound.as_ref(), val)?;
                    ubound = Some(val);
                }
                ([], "eType") => {
                    self.handle_redef("class", "eType", etype.as_ref(), val)?;
                    etype = Some(val);
                }
                ([], "eOpposite") => {
                    self.handle_redef("class", "eOpposite", opposite.as_ref(), val)?;
                    opposite = Some(val);
                }
                ([], "containment") => {
                    self.handle_redef("class", "containment", containment.as_ref(), val)?;
                    containment = Some(val);
                }
                ([], "iD") => {
                    self.handle_redef("class", "iD", is_id.as_ref(), val)?;
                    is_id = Some(val);
                }
                _ => bail!(@unexpected("structural feature attribute") key),
            }

            self.ws();

            if self.try_raw_tag("/>") {
                break 'attributes;
            }
        }

        let name =
            name.ok_or_else(|| error!("illegal structural feature, `name` attribut is missing"))?;
        let typ = repr::structural::Typ::from_xsi_type(typ.as_ref().ok_or_else(|| {
            error!(
                "missing attribute `xsi:type` in structural feature `{}`",
                name
            )
        })?)?;
        let etype = if let Some(etype) = etype {
            ctx.resolve_etype(etype)?
        } else {
            bail!("missing attribute `eType` in structural feature `{}`", name);
        };

        let bounds = typ.parse_bounds(lbound, ubound).context(|| {
            format!(
                "failed to parse `lowerBound`/`upperBound` of  structural feature `{}`",
                name,
            )
        })?;

        let mut structural = repr::Structural::new(name, typ, etype, bounds);

        if let Some(containment) = containment {
            structural.set_containment(helpers::bool(containment)?);
        }
        if let Some(is_id) = is_id {
            structural.set_is_id(helpers::bool(is_id)?);
        }

        if opposite.is_some() {
            log::warn!("`eOpposite` attributes are currently not supported, ignoring")
        }

        Ok(structural)
    }
}
