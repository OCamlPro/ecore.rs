//! This example is a demo of the factory pattern used to represent ecore meta-models.
//!
//! The code pretends to parse the content of [`INPUT`].

ecore_rs::prelude! {
    ctx::PathCtx,
}

pub fn parse_classifier_head<'input>(
    ctx: &mut PathCtx,
    typ: &'input str,
    name: &'input str,
    is_abstract: &'input str,
    super_typs: impl IntoIterator<Item = &'input str>,
) -> Res<idx::Class> {
    match typ {
        "ecore:EClass" => {
            let is_abstract = match is_abstract {
                "true" => true,
                "false" => false,
                _ => bail!(
                    @unknown("boolean value for `abstract` attribute") is_abstract
                ),
            };

            // register class
            let c_idx =
                ctx.add_class(typ, name, Option::<String>::None, Some(is_abstract), None)?;
            // register sub/sup relations
            for sup in super_typs {
                let sup_idx = ctx.class_idx(sup).context(|| {
                    let path = ctx.display_path_sep();
                    format!("resolving super class for `{path}{name}`")
                })?;
                ctx.add_sup_class(sup_idx, c_idx)
            }

            Ok(c_idx)
        }
        _ => bail!(
            @unknown("classifier type") typ
        ),
    }
}

/// Parses the header of a package and makes `ctx` enter this package.
pub fn parse_and_enter_package(ctx: &mut PathCtx, name: &str) -> Res<idx::Pack> {
    ctx.add_and_enter_sub_pack_mut(name)
}
pub fn exit_package(ctx: &mut PathCtx, p_idx: idx::Pack) -> Res<()> {
    let popped = ctx.enter_sup_pack()?;
    debug_assert_eq!(p_idx, popped);
    Ok(())
}

pub fn run(ctx: &mut Ctx) -> Res<()> {
    let mut owned_ctx = ctx.enter_root_pack()?;
    let ctx = &mut owned_ctx;

    let current_pack = parse_and_enter_package(ctx, "laboratory")?;

    parse_classifier_head(ctx, "ecore:EClass", "Person", "true", None)?;
    parse_classifier_head(ctx, "ecore:EClass", "Student", "true", Some("Person"))?;
    parse_classifier_head(ctx, "ecore:EClass", "Teacher", "true", Some("Person"))?;

    exit_package(ctx, current_pack)
}

fn main() {
    println!("running...");
    let mut ctx = Ctx::with_capacity(5, 5);

    run(&mut ctx).expect("run failed :/");

    println!("done\n\nfinal context is\n{}", ctx.to_pretty_string());
}

#[allow(dead_code)]
pub const INPUT: &str = r##"
<?xml version="1.0" encoding="UTF-8"?>
<ecore:EPackage xmi:version="2.0" xmlns:xmi="http://www.omg.org/XMI" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:ecore="http://www.eclipse.org/emf/2002/Ecore" name="laboratory">
  <eClassifiers xsi:type="ecore:EClass" name="Person" abstract="true">
    <eAnnotations source="simpleannotation">
      <details key="simpleannotation" value="Annotation value"/>
    </eAnnotations>
    <eOperations name="setName" eType="ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString">
      <eParameters name="name" lowerBound="1" eType="ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EInt"/>
    </eOperations>
    <eStructuralFeatures xsi:type="ecore:EAttribute" name="name" eType="ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EClass" name="Student" eSuperTypes="#//Person">
    <eStructuralFeatures xsi:type="ecore:EAttribute" name="kind" lowerBound="1" eType="#//StudentKind"/>
    <eStructuralFeatures xsi:type="ecore:EReference" name="advisor" eType="#//Teacher"
        eOpposite="#//Teacher/advises"/>
    <eStructuralFeatures xsi:type="ecore:EReference" name="enrolls" lowerBound="1"
        eType="#//Course"/>
    <eStructuralFeatures xsi:type="ecore:EAttribute" name="StudentID" eType="ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString"
        iD="true"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EClass" name="Teacher" eSuperTypes="#//Person #//Researcher">
    <eStructuralFeatures xsi:type="ecore:EReference" name="advises" upperBound="-1"
        eType="#//Student" eOpposite="#//Student/advisor"/>
    <eStructuralFeatures xsi:type="ecore:EAttribute" name="address" eType="#//Address"/>
    <eStructuralFeatures xsi:type="ecore:EReference" name="teaches" upperBound="-1"
        eType="#//Course"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EEnum" name="StudentKind">
    <eLiterals name="MASTER"/>
    <eLiterals name="PHD" value="1"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EClass" name="Course">
    <eOperations name="subscribe" eType="#//Student"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EDataType" name="Address" instanceTypeName="newDataType2"/>
  <eClassifiers xsi:type="ecore:EClass" name="Laboratory" eSuperTypes="#//ILaboratory">
    <eStructuralFeatures xsi:type="ecore:EReference" name="researchers" upperBound="-1"
        eType="#//Teacher" containment="true"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EClass" name="ILaboratory" abstract="true" interface="true">
    <eOperations name="addMember"/>
  </eClassifiers>
  <eClassifiers xsi:type="ecore:EClass" name="Researcher" abstract="true">
    <eStructuralFeatures xsi:type="ecore:EReference" name="manages" upperBound="-1"
        eType="#//Researcher"/>
  </eClassifiers>
</ecore:EPackage>
"##;
