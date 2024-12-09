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
    "functor", "natural transformation", "monoid", "groupoid", "topos",
    "cartesian closed category", "homoset", "comonad",
    "endofunctor", "fibration", "lilateral morphism", "coequalizer",
    "enriched category", "quiver", "bifunctor", "simplicial object", "sheaf",
    "torsor", "limit", "operad", "part-whole relation", "fusion",
    "tropes", "haecceity",
    "large cardinal", "hyperreal number",
    "constructible universe", "fixed-point combinator"
)

#let fields = (
    "mereotopology", "universalism", "ontic structural realism",
    "modal realism", "zermelo-fraenkel set theory (zf)",
    "intuitionistic logic", "lambda calculus", "higher-order logic",
    "substructural logic", "paraconsistent logic", "algebra of relations",
    "grothendieck topology",
)

#let theorems = (
    "yoneda lemma", "kan extension", "exact sequence principle",
    "spectral sequence", "ontological dependence",
    "truthmaker theory", "modal collapse", "essentialism",
    "counterfactual dependence", "axiom of choice", "ordinal collapse",
    "forcing", "kripke frame", "curry-howard correspondence",
    "predicate abstraction"
)


#let connecting-frase = (
    "implies", "necessarily implies", "only if", "holds provided that", "hold on the condition that", "exists only given that", "whenever", "insofar as",
    "supposing that", "it follows that", "leads to", "if and only if", "is equivalent to", "exactly when", "is coextensive with", 
    "and consequently", "as a direct result", "so by necessity", 
    "unless", "and",  "even if",
)

#let quantifiers = (
    "for all", "there exists", "there is no", "in every case", "in some cases",
)

#let adverbs = (
    "vacuously", "trivially", "logically", "necessarily", "formally",
    "ostensibly","hypothetically", "apparently", "obliquely", "indirectly",
    "superficially", "redundantly", "strictly", "presumably", "nominally",
    "essentially", "fundamentally", "superfluously",
)

#let char_to_int = (char) => {
    ("abcdefghijklmnopqrstuvwxyz").position(char)
}


#let nonsense(body) = {
    let chars = parse-actions(body).filter(char => char != none)

    let nonsentence = (char) => {
        let f = fields.at(calc.rem(char_to_int(char), fields.len()))
        f = upper(f.at(0)) + f.slice(1, f.len())
        let a = adverbs.at(calc.rem(char_to_int(char), adverbs.len()))
        let c = connecting-frase.at(calc.rem(char_to_int(char), connecting-frase.len()))
        let q = quantifiers.at(calc.rem(char_to_int(char), quantifiers.len()))
        let o = objects.at(calc.rem(char_to_int(char), objects.len()))
        let l = theorems.at(calc.rem(char_to_int(char), theorems.len()))

        [#f #a #c #q #o the #l holds.]
    }

    chars.map(char => nonsentence(char)).join(" ")
}


// GEN 1 NONSENSE CONSTRUCTOR
#nonsense[e]


