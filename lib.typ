#set text(size: 12pt)
#set page(margin: (x: 20%))

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

#let vars = ("x", "y", "z", $theta$)
#let funcs = ("f", $lambda$, "g", "G", $Im$)
#let joiner = ($and$, $or$, $xor$)
#let alphabet = "abcdefghijklmnopqrstuvwxyz"
#let to-int = (char) => {("ab*()&^%$#@!'cd:;efghijklmnopqrstuvwxyz").position(char)}
#let get = (arr, i) => {arr.at(calc.rem(i, arr.len()))}
#let cap = (str) => [#upper(str.at(0))#str.slice(1, str.len())]


#let objects = (
    "functor", "transformation", "monoid", "groupoid", "topos", "cartesian
    closed category", "homoset", "comonad", "endofunctor", "fibration",
    "lateral morphism", "coequalizer", "category", "quiver", "bifunctor",
    "object", "sheaf", "torsor", "limit", "operad", "part-whole relation",
    "fusion", "haecceity", "subspace", "ordinal", "large cardinal", 
    "hyperreal number", "universe", "combinator"
)

#let buzzwords = (
    "abstract", "relational", "substructural", "discrete", "inerpolated",
    "intuitional", "higher order", "paraconsistent", "interrelational",
    "structural", "ontic", "semi ontic", "modal", "formal", "informal", "psuedo", 
    "natural", "enriched", "simplicial", "abelian", "constructable", "fixed point"
)

#let fields = (
    "calculus", "statistics", "logic", "algebra", "set theory", "topology",
    "ontology","mereology"
)

#let theorems = (
    "yoneda lemma", "kan extension", "exact sequence principle",
    "spectral sequence lemma",
    "truthmaker theory", 
    "modal collapse", "essentialism",
    "counterfactual dependence theorem", "axiom of choice", "ordinal collapse",
    "kripke frame", "curry-howard correspondence",
    "predicate abstraction"
)

#let feild = (i) => {
    let b1 = get(buzzwords, i)
    let b2 = get(buzzwords, i + 2)
    let f = get(fields, i)
    [#b1 #b2 #f]
}


#let last_names = (
    "Euler", "Bernstein", "Schröder", "Pascal", "Descartes", "Gödel", "Turing",
    "Cantor", "Fibonacci", "Leibniz", "Pythagoras", "Noether", "Hilbert",
    "Russell", "Frege", "Zeno", "Curry" 
);


#let theorem = (i) => {
    let o = get(objects, i)
    let b = get(buzzwords, i)
    let a = if calc.rem(i, 2) == 0 {get(last_names, i)} else {get(buzzwords, i - 2)}
    let k = get(
    ("lemma", "theorem", "axiom", "conjecture", "principle", "extension",
    "theory"), i)

    [the #b #a #o #k]
}

#let connecting-frase = (
    "implies", "necessarily implies", "only if", "holds provided that", "hold
    on the condition that", "exists only given that", "whenever", "insofar as",
    "supposing that", "it follows that", "leads to", "if and only if", "is
    equivalent to", "exactly when", "is coextensive with", "and consequently",
    "as a direct result", "so by necessity", "unless", "and",  "even if",
)

#let quantifiers = (
    $forall$, $exists$ 
)

#let connectives = (
    $==>$, $-->$, $<==>$, $arrow.double.not$, $arrow.squiggly$, $eq.triple$ 
)

#let adverbs = (
    "vacuously", "trivially", "logically", "necessarily", "formally",
    "ostensibly","hypothetically", "apparently", "obliquely", "indirectly",
    "superficially", "redundantly", "strictly", "presumably", "nominally",
    "essentially", "fundamentally", "superfluously",
)

#let openers = ("a problem arises", "a long sought after result", "a common challenge may be")


#let nonsense(body) = {
    let chars = parse-actions(body).filter(char => char != none)
    if chars.len() == 0 {
        return
    }

    let non-statement = (i) => {
        let q = get(quantifiers, i)
        let f = get(funcs, i)
        let f2 = get(funcs, i + 1)
        let v = get(vars, i)
        let v2 = get(vars, i - 1)
        let c = get(connectives, i)
        [[#q #v2#f\(#v)#c#f2\(#v2)]]
    }

    let non-introduction = (i) => {
        let o = get(openers, i)
        let f = feild(i)
        [In #f #o]
    }

    let glob-i = chars.map(c => to-int(c)).sum()
    let theorem = theorem(glob-i)
    let object = get(objects, glob-i)


    align(center)[= Proving #theorem for an arbitrary #object]
    [\ ]

    par(first-line-indent: 1em)[
        #{for (i, c) in chars.enumerate() {
            let n = to-int(c)

            if i == 0 {
                [#non-introduction(n)]
            } else {
                [#non-statement(n)]
            }
            [ ]
        }}
    ]
}

#nonsense[m]
