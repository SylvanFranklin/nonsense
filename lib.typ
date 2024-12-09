#set text(size: 12pt)

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
#let to-int = (char) => {("abcdefghijklmnopqrstuvwxyz").position(char)}
#let get = (arr, i) => {arr.at(calc.rem(i, arr.len()))}
#let cap = (str) => [#upper(str.at(0))#str.slice(1, str.len())]


#let objects = (
    "functor", "natural transformation", "monoid", "groupoid", "topos",
    "cartesian closed category", "homoset", "comonad",
    "endofunctor", "fibration", "lateral morphism", "coequalizer",
    "enriched category", "quiver", "bifunctor", "simplicial object", "sheaf",
    "torsor", "limit", "operad", "part-whole relation", "fusion",
    "tropes", "haecceity",
    "large cardinal", "hyperreal number",
    "constructible universe", "fixed-point combinator"
)


#let feild = (i) => {
    let buzzwords = (
        "abstract", "relational", "substructural", "discrete", "inerpolated",
        "intuitional", "higher order", "paraconsistent", "interrelational",
        "structural", "ontic", "semi ontic", "modal", "formal", "informal"
    )

    let fields = (
        "calculus", "statistics", "logic", "algebra", "set theory", "topology",
        "ontology","mereology"
    )
    let b1 = get(buzzwords, i)
    let b2 = get(buzzwords, i + 2)
    let f = get(fields, i)

    [#b1 #b2 #f]
}


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

    let nonsentence = (char) => {
        let i = to-int(char)
        let j = get(joiner, i)
        let a = get(adverbs, i)

        [#a #non-statement(i)#j#non-statement(i - 1)]
    }
    let n = chars.map(c => to-int(c)).sum()
    let theorem = get(theorems, n)
    let object = get(objects, n)

    align(center)[= Proving #theorem for an arbitrary #object]
    [\ ]
    for (i, c) in chars.enumerate() {
        let n = to-int(c)

        if i == 0 {
            [#non-introduction(n)]
        } else {
            [#non-statement(n)]
        }

        [ ]
        
    }

}

#nonsense[x]
