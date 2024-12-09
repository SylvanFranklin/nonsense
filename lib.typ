#let parse-actions(body) = {
  let extract(it) = {
    ""
    if it == [ ] {
      " "
    } else if it.func() == text {
      it.text
    } else if it.func() == [].func() {
      it.children.map(extract).join()
    }
  }
  extract(body).clusters().map(lower)
}


#let objects = (
    "Functor", "Natural transformation", "Monoid", "Groupoid", "Topos",
    "Pullback / Pushout", "Cartesian closed category", "Hom-set", "Comonad",
    "Endofunctor", "Fibration", "Lax morphism", "Coequalizer",
    "Enriched category", "Quiver", "Bifunctor", "Simplicial object", "Sheaf",
    "Torsor", "Colimit / Limit", "Operad", "Part-whole relation", "Fusion",
    "Gunk (mereological)", "Tropes", "Haecceity", "Relational ontology",
    "Large cardinal", "Cardinality", "Hyperreal number",
    "Constructible universe", "Boolean algebra"
)

#let fields = (
    "Infinity category", "Decategorification", "Mereotopology",
    "Universalism (mereology)", "Ontic structural realism", "Modal realism",
    "Substratum", "Zermelo-Fraenkel set theory (ZF)", "Intuitionistic logic",
    "Lambda calculus", "Higher-order logic", "Substructural logic",
    "Paraconsistent logic", "Algebra of relations"
)

#let theorems_lemmas_rules = (
    "Yoneda Lemma", "Adjunction", "Kan extension", "Exact sequence",
    "Spectral sequence", "Derived functor", "Grothendieck topology",
    "Skolemization", "Supervenience", "Ontological dependence",
    "Truthmaker theory", "Modal collapse", "Essentialism",
    "Counterfactual dependence", "Axiom of choice", "Ordinal collapse",
    "Forcing", "Kripke frame", "Curry-Howard correspondence",
    "Predicate abstraction", "Fixed-point combinator", "Sequent calculus",
    "Ultraproduct", "Dialetheism"
)


#let connecting-frase = (
    "Implies", "Necessarily implies", "Only if", "Provided that", "On the
    condition that", "Given that", "Whenever", "Entails that", "Insofar as",
    "Supposing that", "It follows that", "Leads to", "If and only if", "Just in
    case", "Is equivalent to", "Exactly when", "Necessary and sufficient
    condition", "Coextensive with", "Because", "Therefore", "Thus", "Hence",
    "Consequently", "As a result", "By necessity", "It must be that", "Or",
    "Either ", "Unless", "Otherwise", "And", "Moreover", "Furthermore",
    "Additionally", "As well as", "While", "Even if",
)

#let quantifiers = (
    "For all", "There exists", "There is no", "Such that", "In every case", "In
    some cases",
)

#let adverbs = (
    "Vacuously", "Trivially", "Logically", "Necessarily", "Formally",
    "Ostensibly","Hypothetically", "Apparently", "Obliquely", "Indirectly",
    "Superficially", "Redundantly", "Strictly", "Presumably", "Nominally",
    "Essentially", "Fundamentally", "Superfluously",
)

#let char_to_int = (char) => {
    ("abcdefghijklmnopqrstuvwxyz").position(char)
}


#let nonsense(body) = {
    let chars = parse-actions(body).filter(char => char != none)

    let nonsentence = (char) => {
        fields.at(calc.rem(char_to_int(char), fields.len()))
    }

    chars.map(char => nonsentence(char)).join(" ")
}

#nonsense()[enaio]
