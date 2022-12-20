// #![allow(clippy::result_large_err)]
use xmlparser::{ElementEnd, StrSpan, Stream, TextPos, Token};

prelude! {}
use crate::{
    ctx::{ClassCtx, PathCtx},
    prelude::res::Error as PrError,
    repr::{structural::Typ, Annot, Operation},
};

#[derive(Debug)]
pub enum Error<'stream, E> {
    NoMoreTokens,
    LexingError {
        inner: xmlparser::Error,
    },
    UnexpectedToken {
        textpos: TextPos,
        expected: &'static str,
        found: &'stream str,
    },
    BoolParseError {
        str_span: StrSpan<'stream>,
        stream: Stream<'stream>,
    },
    MultipleETypesParseError {
        str_span: StrSpan<'stream>,
        stream: Stream<'stream>,
    },
    MissingExpected {
        expected: &'static str,
        textpos: TextPos,
    },
    VisitorError {
        inner: E,
        textpos: TextPos,
    },
}

fn textpos_from_token(stream: &Stream<'_>, token: &Token<'_>) -> TextPos {
    let span = token.span();
    stream.gen_text_pos_from(span.start())
}

impl<'stream, E: std::fmt::Debug> std::fmt::Display for Error<'stream, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NoMoreTokens => write!(f, "Premature end of file"),
            Error::LexingError { inner } => write!(f, "There was an error lexing the XML: {inner}"),
            Error::UnexpectedToken {
                textpos,
                expected,
                found,
            } => {
                let col = textpos.col;
                let line = textpos.row;
                write!(
                    f,
                    "Unexpected token \"{found}\" line {line}, col {col}. Expected: {expected}"
                )
            }
            Error::MissingExpected { expected, textpos } => {
                let col = textpos.col;
                let line = textpos.row;
                write!(
                    f,
                    "Missing expected expected {expected} line {line}, col {col}."
                )
            }
            Error::VisitorError { inner, textpos } => {
                let col = textpos.col;
                let line = textpos.row;
                write!(f, "Inner visitor error: {inner:?} line {line} col {col}")
            }
            Error::BoolParseError { str_span, stream } => {
                let s = str_span.as_str();
                let textpos = stream.gen_text_pos_from(str_span.start());
                let col = textpos.col;
                let line = textpos.row;
                write!(
                    f,
                    "Cannot interpret \"{s}\" as a boolean, line {line}, col {col}."
                )
            }
            Error::MultipleETypesParseError { str_span, stream } => {
                let s = str_span.as_str();
                let textpos = stream.gen_text_pos_from(str_span.start());
                let col = textpos.col;
                let line = textpos.row;
                write!(
                    f,
                    "Cannot parse eTypes from \"{s}\", line {line}, col {col}."
                )
            }
        }
    }
}

impl<'stream, E> Error<'stream, E> {
    fn unexpected_token(
        tkn: Token<'stream>,
        stream: Stream<'stream>,
        expected: &'static str,
    ) -> Self {
        let textpos = textpos_from_token(&stream, &tkn);
        Self::UnexpectedToken {
            textpos,
            expected,
            found: tkn.span().as_str(),
        }
    }
}

fn parse_xml_bool<'stream, V>(
    s: StrSpan<'stream>,
    stream: Stream<'stream>,
) -> WalkResult<'stream, bool, V> {
    match s.as_str() {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(Error::BoolParseError {
            str_span: s,
            stream,
        }),
    }
}

fn parse_e_types<'s>(mut s: &'s str, acc: &mut Vec<&'s str>) -> Result<(), ()> {
    while {
        s = s.trim_start();
        !s.is_empty()
    } {
        let index = if s.starts_with("#//") {
            s.find(char::is_whitespace).unwrap_or(s.len())
        } else if s.starts_with("ecore:EDataType") {
            let mut copy_s = s;
            // to ensure that s is not used inside this branch
            // and only copy_s is.
            #[allow(unused)]
            let s = 0_u8;
            let mut idx = 0;
            match copy_s.find(char::is_whitespace) {
                Some(i) => {
                    idx += i;
                    copy_s = copy_s.split_at(i).1;
                }
                None => return Err(()),
            }
            {
                let i = copy_s
                    .char_indices()
                    .take_while(|(_, c)| c.is_whitespace())
                    .last()
                    .unwrap_or_else(|| unreachable!())
                    .0;
                idx += i + 1;
                copy_s = copy_s.split_at(i + 1).1;
            }
            {
                let i = copy_s.find(char::is_whitespace).unwrap_or(copy_s.len());
                idx += i;
            }
            idx
        } else {
            return Err(());
        };
        let (to_push, rem) = s.split_at(index);
        acc.push(to_push);
        s = rem;
    }
    Ok(())
}

impl<'stream, E: std::fmt::Debug> std::error::Error for Error<'stream, E> {}

impl<E> From<xmlparser::Error> for Error<'_, E> {
    fn from(inner: xmlparser::Error) -> Self {
        Error::LexingError { inner }
    }
}

pub type WalkResult<'a, T, E> = Result<T, Error<'a, E>>;

#[derive(Debug, Clone)]
pub struct ECoreWalker<'a> {
    inner: xmlparser::Tokenizer<'a>,
}

trait Annotable {
    fn add_annotation(&mut self, annot: Annot);
}

impl Annotable for ClassCtx<'_, '_> {
    fn add_annotation(&mut self, annot: Annot) {
        ClassCtx::add_annotation(self, annot)
    }
}

impl Annotable for PathCtx<'_> {
    fn add_annotation(&mut self, annot: Annot) {
        PathCtx::add_annotation(self, annot)
    }
}

impl<'a> ECoreWalker<'a> {
    pub fn new(inner: xmlparser::Tokenizer<'a>) -> Self {
        Self { inner }
    }

    fn next_token<E>(&mut self) -> WalkResult<'a, Token<'a>, E> {
        loop {
            let tkn = self.inner.next().ok_or(Error::NoMoreTokens)??;
            if !matches!(tkn, Token::Text { .. }) {
                return Ok(tkn);
            }
        }
    }

    pub fn start_visit(&mut self, ctx: &mut Ctx) -> WalkResult<'a, (), PrError> {
        let mut patch_ctx = ctx.enter_root_pack().map_err(|inner| Error::VisitorError {
            inner,
            textpos: self.stream().gen_text_pos(),
        })?;
        let tkn = self.next_token()?;
        match tkn {
            xmlparser::Token::Declaration { version, .. } => {
                assert_eq!(version.as_str(), "1.0");
            }
            _ => {
                return Err(Error::unexpected_token(
                    tkn,
                    self.stream(),
                    "XML declaration",
                ));
            }
        };
        match self.next_token()? {
            Token::ElementStart { prefix, local, .. }
                if prefix == "ecore" && local == "EPackage" =>
            {
                self.walk_package_inner(&mut patch_ctx)?;
            }
            tkn => {
                return Err(Error::unexpected_token(
                    tkn,
                    self.stream(),
                    "begining of ecore:EPackage",
                ));
            }
        }
        match self.next_token() {
            Err(Error::NoMoreTokens) => Ok(()),
            Ok(tkn) => Err(Error::unexpected_token(tkn, self.stream(), "End of file")),
            Err(e) => Err(e),
        }
    }

    fn walk_package_inner(&mut self, pctx: &mut PathCtx) -> WalkResult<'a, (), PrError> {
        let mut found_name = false;
        loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && "name" == local => {
                    pctx.add_and_enter_sub_pack_mut(value.as_str())
                        .map_err(|inner| Error::VisitorError {
                            inner,
                            textpos: self.stream().gen_text_pos(),
                        })?;
                    found_name = true;
                }
                Token::Attribute { .. } => continue,
                Token::ElementEnd {
                    end: ElementEnd::Open,
                    ..
                } => {
                    if found_name {
                        break;
                    } else {
                        return Err(Error::MissingExpected {
                            expected: "name attribute for ecore:EPackage element",
                            textpos: self.stream().gen_text_pos(),
                        });
                    }
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "name attribute for ecore:EPackage element",
                    ))
                }
            }
        }
        loop {
            let tkn = self.next_token()?;

            match tkn {
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eAnnotations" =>
                {
                    self.walk_annotations_inner(pctx)?
                }
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eClassifiers" =>
                {
                    self.walk_classifiers_inner(pctx)?
                }
                Token::ElementStart { prefix, local, .. }
                    if prefix == "ecore" && local == "EPackage" =>
                {
                    self.walk_package_inner(pctx)?
                }
                Token::ElementEnd {
                    end: ElementEnd::Close(prefix, local),
                    ..
                } if prefix == "ecore" && local == "EPackage" => {
                    pctx.enter_sup_pack().map_err(|inner| Error::VisitorError {
                        inner,
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "inside an ePackage, expecting opening of \
                        1) an eAnnotation 2) an ePackage or 3) an eClassifier",
                    ))
                }
            };
        }
        Ok(())
    }

    /// Get a reference to the ecore walker's stream.
    pub fn stream(&self) -> Stream<'a> {
        self.inner.stream()
    }

    fn walk_classifiers_inner(&mut self, pctx: &mut PathCtx) -> WalkResult<'a, (), PrError> {
        let mut xsi_type = None;
        let mut name = None;
        let mut r#abstract = None;
        let mut e_super_types = Vec::new();
        let mut instance_type_name = None;
        let mut interface = None;
        let mut class = loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "interface" => {
                    interface = Some(parse_xml_bool(value, self.stream())?)
                }
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "name" => name = Some(value.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "instanceTypeName" => {
                    instance_type_name = Some(value.as_str())
                }
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "eSuperTypes" => {
                    parse_e_types(value.as_str(), &mut e_super_types).map_err(|()| {
                        Error::MultipleETypesParseError {
                            str_span: value,
                            stream: self.stream(),
                        }
                    })?;
                }
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix == "xsi" && local == "type" => xsi_type = Some(value.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "abstract" => {
                    r#abstract = Some(parse_xml_bool(value, self.stream())?)
                }
                Token::ElementEnd { end, .. } => {
                    let name = name.ok_or(Error::MissingExpected {
                        expected: "name attribute for eClassifier element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let xsi_type = xsi_type.ok_or(Error::MissingExpected {
                        expected: "xsi:type attribute for eClassifier element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let mut class = pctx
                        .enter_class(xsi_type, name, instance_type_name, r#abstract, interface)
                        .map_err(|inner| Error::VisitorError {
                            inner,
                            textpos: self.stream().gen_text_pos(),
                        })?;
                    for estp in e_super_types {
                        let estp =
                            class
                                .resolve_etype(estp)
                                .map_err(|inner| Error::VisitorError {
                                    inner,
                                    textpos: self.stream().gen_text_pos(),
                                })?;
                        class.add_sup_class(estp);
                    }
                    match end {
                        ElementEnd::Open => break class,
                        ElementEnd::Close(_, _) => {
                            return Err(Error::unexpected_token(
                                tkn,
                                self.stream(),
                                "> or /> to close start of eClassifier",
                            ))
                        }
                        ElementEnd::Empty => {
                            return Ok(());
                        }
                    }
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "xsi:type, name or abstract attribute for eClassifier element",
                    ))
                }
            }
        };
        loop {
            let tkn = self.next_token()?;

            match tkn {
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eLiterals" =>
                {
                    self.walk_literal_inner(&mut class)?
                }
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eAnnotations" =>
                {
                    self.walk_annotations_inner(&mut class)?
                }
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eOperations" =>
                {
                    self.walk_operations_inner(&mut class)?
                }
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eStructuralFeatures" =>
                {
                    self.walk_structural_features_inner(&mut class)?
                }
                Token::ElementEnd {
                    end: ElementEnd::Close(prefix, local),
                    ..
                } if prefix.is_empty() && local == "eClassifiers" => break,
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "inside an eClassifier, expecting opening of \
                        1) an eOperations 2) an eAnnotation or 2) a structural feature",
                    ))
                }
            };
        }
        Ok(())
    }

    fn walk_annotations_inner<AnnotableCtx: Annotable>(
        &mut self,
        ctx: &mut AnnotableCtx,
    ) -> WalkResult<'a, (), PrError> {
        let mut annot = None;
        loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "source" => {
                    annot = Some(Annot::with_capacity(value.as_str(), 0));
                }
                Token::Attribute { .. } => continue,
                Token::ElementEnd {
                    end: ElementEnd::Open,
                    ..
                } => {
                    if annot.is_some() {
                        break;
                    } else {
                        return Err(Error::MissingExpected {
                            expected: "source attribute for eAnnotation element",
                            textpos: self.stream().gen_text_pos(),
                        });
                    }
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "source attribute for eAnnotation element",
                    ))
                }
            }
        }
        let mut annot = annot.unwrap_or_else(|| unreachable!());
        loop {
            let tkn = self.next_token()?;

            match tkn {
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "details" =>
                {
                    self.walk_details_inner(&mut annot)?
                }
                Token::ElementEnd {
                    end: ElementEnd::Close(prefix, local),
                    ..
                } if prefix.is_empty() && local == "eAnnotations" => {
                    ctx.add_annotation(annot);
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "inside an eAnnotation, expecting opening details",
                    ))
                }
            };
        }
        Ok(())
    }
    fn walk_operations_inner(&mut self, classctx: &mut ClassCtx) -> WalkResult<'a, (), PrError> {
        let mut name = None;
        let mut e_type = None;
        let mut operation = loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "name" => name = Some(value.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "eType" => e_type = Some(value.as_str()),
                Token::ElementEnd { end, .. } => {
                    let name = name.ok_or(Error::MissingExpected {
                        expected: "name attribute for eOperations element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let e_type = if let Some(e_type) = e_type {
                        Some(classctx.resolve_etype(e_type).map_err(|inner| {
                            Error::VisitorError {
                                inner,
                                textpos: self.stream().gen_text_pos(),
                            }
                        })?)
                    } else {
                        None
                    };
                    let operation = Operation::with_capacity(name, e_type, 0);
                    // vis.enter_operations(name, e_type)
                    //     .map_err(|inner| Error::VisitorError {
                    //         inner,
                    //         textpos: self.stream().gen_text_pos(),
                    //     })?;
                    match end {
                        ElementEnd::Open => break operation,
                        ElementEnd::Close(_, _) => {
                            return Err(Error::unexpected_token(
                                tkn,
                                self.stream(),
                                "> or /> to close start of eOperations",
                            ))
                        }
                        ElementEnd::Empty => {
                            return Ok(());
                        }
                    }
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "eType or name attribute for eClassifier element",
                    ))
                }
            }
        };
        loop {
            let tkn = self.next_token()?;

            match tkn {
                Token::ElementStart { prefix, local, .. }
                    if prefix.is_empty() && local == "eParameters" =>
                {
                    self.walk_parameters_inner(classctx, &mut operation)?
                }
                Token::ElementEnd {
                    end: ElementEnd::Close(prefix, local),
                    ..
                } if prefix.is_empty() && local == "eOperations" => {
                    classctx.add_operation(operation);
                    // vis.exit_operations().map_err(|inner| Error::VisitorError {
                    //     inner,
                    //     textpos: self.stream().gen_text_pos(),
                    // })?;
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "inside an eOperations, expecting opening an eParameters",
                    ))
                }
            };
        }
        Ok(())
    }

    fn walk_details_inner(&mut self, annot: &mut Annot) -> WalkResult<'a, (), PrError> {
        let mut key = None;
        let mut value = None;
        loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "key" => key = Some(value.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "value" => value = Some(val.as_str()),
                Token::ElementEnd {
                    end: ElementEnd::Empty,
                    ..
                } => {
                    let key = key.ok_or(Error::MissingExpected {
                        expected: "key attribute for details element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let value = value.ok_or(Error::MissingExpected {
                        expected: "value attribute for details element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    annot
                        .insert(key, value)
                        .map_err(|inner| Error::VisitorError {
                            inner,
                            textpos: self.stream().gen_text_pos(),
                        })?;
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "key or value attribute for details element",
                    ))
                }
            }
        }
        Ok(())
    }
    fn walk_structural_features_inner(
        &mut self,
        classctx: &mut ClassCtx,
    ) -> WalkResult<'a, (), PrError> {
        let mut name = None;
        let mut e_type = None;
        let mut xsi_type = None;
        let mut lower_bound = None;
        let mut upper_bound = None;
        let mut e_opposite = None;
        let mut i_d = None;
        let mut containment = None;
        loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "containment" => {
                    containment = Some(parse_xml_bool(value, self.stream())?)
                }
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "iD" => {
                    i_d = Some(parse_xml_bool(value, self.stream())?)
                }
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "eOpposite" => e_opposite = Some(val.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "lowerBound" => lower_bound = Some(val.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "upperBound" => upper_bound = Some(val.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "name" => name = Some(value.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix == "xsi" && local == "type" => {
                    xsi_type = {
                        let kind = match val.as_str() {
                            "ecore:EReference" => Typ::EReference,
                            "ecore:EAttribute" => Typ::EAttribute,
                            _ => {
                                return Err(Error::unexpected_token(
                                    tkn,
                                    self.stream(),
                                    r#"xsi:type attribute should be either "ecore:EReference" or "ecore:EAttribute""#,
                                ))
                            }
                        };
                        Some(kind)
                    }
                }
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "eType" => e_type = Some(val.as_str()),
                Token::ElementEnd {
                    end: ElementEnd::Empty,
                    ..
                } => {
                    let name = name.ok_or(Error::MissingExpected {
                        expected: "name attribute for eStructuralFeatures element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let e_type = e_type.ok_or(Error::MissingExpected {
                        expected: "eType attribute for eStructuralFeatures element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let xsi_type = xsi_type.ok_or(Error::MissingExpected {
                        expected: "xsi:type attribute for eStructuralFeatures element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    if e_opposite.is_some() {
                        log::warn!("`eOpposite` attributes are currently not supported, ignoring")
                    }
                    let bounds =
                        xsi_type
                            .parse_bounds(lower_bound, upper_bound)
                            .map_err(|inner| Error::VisitorError {
                                inner,
                                textpos: self.stream().gen_text_pos(),
                            })?;
                    let typ =
                        classctx
                            .resolve_etype(e_type)
                            .map_err(|inner| Error::VisitorError {
                                inner,
                                textpos: self.stream().gen_text_pos(),
                            })?;
                    let mut structural = repr::Structural::new(name, xsi_type, typ, bounds);
                    if let Some(b) = containment {
                        structural.set_containment(b);
                    }
                    if let Some(b) = i_d {
                        structural.set_is_id(b);
                    }
                    classctx.add_structural(structural);
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "name, eType or xsi:type attribute for eStructuralFeatures element",
                    ))
                }
            }
        }
        Ok(())
    }

    fn walk_parameters_inner(
        &mut self,
        classctx: &mut ClassCtx,
        ope: &mut Operation,
    ) -> WalkResult<'a, (), PrError> {
        let mut name = None;
        let mut lower_bound = None;
        let mut upper_bound = None;
        let mut e_type = None;
        loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "name" => name = Some(value.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "lowerBound" => lower_bound = Some(val.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "upperBound" => upper_bound = Some(val.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "eType" => e_type = Some(val.as_str()),
                Token::ElementEnd {
                    end: ElementEnd::Empty,
                    ..
                } => {
                    let name = name.ok_or(Error::MissingExpected {
                        expected: "name attribute for eParameter element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let e_type = {
                        let e_type = e_type.ok_or(Error::MissingExpected {
                            expected: "eType attribute for eParameter element",
                            textpos: self.stream().gen_text_pos(),
                        })?;
                        classctx
                            .resolve_etype(e_type)
                            .map_err(|inner| Error::VisitorError {
                                inner,
                                textpos: self.stream().gen_text_pos(),
                            })?
                    };
                    let bounds =
                        repr::Bounds::from_str(lower_bound, upper_bound).map_err(|inner| {
                            Error::VisitorError {
                                inner,
                                textpos: self.stream().gen_text_pos(),
                            }
                        })?;
                    let param = repr::Param::new(name, bounds, e_type);
                    ope.add_parameter(param);
                    // vis.do_parameters(name, upper_bound, lower_bound, e_type)
                    //     .map_err(|inner| Error::VisitorError {
                    //         inner,
                    //         textpos: self.stream().gen_text_pos(),
                    //     })?;
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "key or value attribute for eParameters element",
                    ))
                }
            }
        }
        Ok(())
    }
    fn walk_literal_inner(&mut self, class_ctx: &mut ClassCtx) -> WalkResult<'a, (), PrError> {
        let mut name = None;
        let mut value = None;
        loop {
            let tkn = self.next_token()?;
            match tkn {
                Token::Attribute {
                    prefix,
                    local,
                    value: val,
                    ..
                } if prefix.is_empty() && local == "value" => value = Some(val.as_str()),
                Token::Attribute {
                    prefix,
                    local,
                    value,
                    ..
                } if prefix.is_empty() && local == "name" => name = Some(value.as_str()),
                Token::ElementEnd {
                    end: ElementEnd::Empty,
                    ..
                } => {
                    let name = name.ok_or(Error::MissingExpected {
                        expected: "name attribute for eLiteral element",
                        textpos: self.stream().gen_text_pos(),
                    })?;
                    let lit = repr::ELit::new(name, value);
                    class_ctx.add_literal(lit);
                    // vis.do_literal(name, value)
                    //     .map_err(|inner| Error::VisitorError {
                    //         inner,
                    //         textpos: self.stream().gen_text_pos(),
                    //     })?;
                    break;
                }
                _ => {
                    return Err(Error::unexpected_token(
                        tkn,
                        self.stream(),
                        "name, or value attribute for literal element",
                    ))
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::parse_e_types;

    #[test]
    fn parse_e_types1() {
        let mut acc = Vec::new();
        parse_e_types("#//Person", &mut acc).unwrap();
        assert_eq!(acc, vec!["#//Person"])
    }

    #[test]
    fn parse_e_types2() {
        let mut acc = Vec::new();
        parse_e_types(
            "ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString",
            &mut acc,
        )
        .unwrap();
        assert_eq!(
            acc,
            vec!["ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString"]
        )
    }

    #[test]
    fn parse_e_types3() {
        let mut acc = Vec::new();
        let s = "  #//Person ecore:EDataType \
        http://www.eclipse.org/emf/2002/Ecore#//EString   
        #//Foo  
        ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString  ";
        parse_e_types(s, &mut acc).unwrap();
        assert_eq!(
            acc,
            vec![
                "#//Person",
                "ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString",
                "#//Foo",
                "ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString"
            ]
        )
    }
}
