# ecore.rs

A parser and internal representation for the [Ecore format][ecore] used by the Eclipse Modeling
Framework (EMF).



## TODO

- [ ] investigate type parameters for classes
- [ ] investigate type parameters for operations
- [ ] builtin type constructors (`Vec`, `Option`, ...)?



## Input (XML) format

A few examples are available in the [rsc](./rsc) folder, both XML and JPG.

We currently support the following abstract layout

```ebnf
file =
    xml_version (package)*

package =
    "<ecore:Package" ... "name" "=" string ">"
        (annotation | package | classifier)*
    "</ecore:Package>"

# a class (concrete/abstract/enum/...)
classifier =
    "<eClassifiers"
        "xsi:type" "=" string
        "name" "=" string
        "abstract" "=" string
    ">"
        (annotation | operation | structural_feature)
    "</eClassifiers>

annotation =
    "<eAnnotations"
        "source" "=" string
    ">"
        ( "<details" "key" "=" string "value" "=" string ">" )*
    "</eAnnotations>"

# a method
operation =
    "<eOperations"
        "name" "=" string
        "eType" "=" string
    ">"
        (
            "<eParameters"
                "name" "=" string
                "lowerBound" "=" nat_string
                "upperBound" "=" int_string
                "eType" "=" string
            ">"
        )*
    "</eOperations>"

structural_feature =
    "<eStructuralFeatures"
        "xsi:type" "=" string
        "name" "=" string
        "eType" "=" string
    "/>"
```


[ecore]: https://download.eclipse.org/modeling/emf/emf/javadoc/2.9.0/org/eclipse/emf/ecore/package-summary.html#details