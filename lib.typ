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
#let vowels = "aeiouy"
#let to-int = (char) => {("ab*()&^%$#@!'cd:;efghijklmnopqrstuvwxyz").position(char)}
#let get = (arr, i) => {arr.at(calc.rem(i, arr.len()))}
#let cap = (str) => [#upper(str.at(0))#str.slice(1, str.len())]
#let sing = (str) => {if str.at(0) in vowels [an #str] else [a #str]}

#let objects = (
    "functor", "transformation", "monoid", "groupoid", "topos", 
    "closed category", "homoset", "comonad", "endofunctor", "fibration",
    "lateral morphism", "coequalizer", "category", "quiver", "bifunctor",
    "object", "sheaf", "torsor", "limit", "operad", "part-whole relation",
    "fusion", "haecceity", "subspace", "ordinal", "large cardinal", 
    "hyperreal number", "universe", "combinator"
)

#let symbols = (
    meta-cyclic-integral: $integral.cont.ccw$,
    combinator: $lambda Epsilon_1$ ,
    composition: $f circle.small g$,
    section: $section$ 
)

#let buzzwords = (
    "abstract", "relational", "substructural", "discrete", "inerpolated",
    "intuitional", "higher order", "paraconsistent", "interrelational",
    "structural", "ontic", "semi ontic", "modal", "formal", "informal", "psuedo", 
    "natural", "enriched", "simplicial", "abelian", "constructable", "fixed point", 
    "euclidian", "anti", "meta", "stochastically", "bijective"
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

#let last_names = (
    "Euler", "Bernstein", "Schröder", "Pascal", "Samuel", "Gödel", "Nozzle",
    "Cantor", "Jones", "Pythis", "Noether", "Rubble", "Russell", "Frege",
    "Zeno", "Curry", "Franklin", "Wager", "Pappas", "Fawkes", "Baccus",
    "Lancaster", "Zilber", "Abou",
);

#let field = (i) => {
    let b1 = get(buzzwords, i)
    let b2 = get(buzzwords, i + 2)
    let f = get(fields, i)
    [#b1 #b2 #f]
}

#let authors = (i) => {
    // we will make between one and three authors 
    range(0, calc.rem(i, 4) + 1).map(n => 
        [#cap(get(alphabet, i + n)). #get(last_names, i + n)]
    ).join(", ")
}

#let theorem = (i) => {
    let o = get(objects, i)
    let b = get(buzzwords, i)
    let a = if calc.rem(i, 2) == 0 {
        get(last_names, i)
    } else {
        get(buzzwords, i - 2)
    }

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
#let verbs = (
    "generalizing", "proving", "demonstrating", "containing",
    "defining", "constructing"
)

#let openers = (
    "a problem arises", "a long sought after result", "a common challenge may be")


#let nonsense(body) = {
    let chars = parse-actions(body).filter(char => char != none)
    if chars.len() == 0 { return }
    let glob-i = chars.map(c => to-int(c)).sum()
    let thm1 = theorem(glob-i)
    let thm2 = theorem(glob-i + 1)
    let obj1 = get(objects, glob-i)
    let obj2 = get(objects, glob-i + 1)
    let obj3 = get(objects, glob-i + 2)

    let debug = () => {
        let point-pair = (c) => $vec(delim: "[", #c, #text(blue)[#to-int(c)])$
        block(inset: 1em, stroke: 0.1em, radius: 1em, width: 100%)[
            *seed* : #{
                if chars.len() < 5 {
                    [#chars.map(c => point-pair(c)).join(" + ") = #glob-i]
                } else {
                    [#chars.slice(0, 3).map(c => point-pair(c)).join(" + ") + ... + 
                    #point-pair(chars.last()) = #glob-i]
                }
            }
        ]
    }
    
    let non-statement = (i) => {
        let o = get(symbols.keys(), i)
        [Consider #sing(o) (#symbols.at(o))] 
    }

    let non-introduction = (i) => {
        let o = get(openers, i)
        let f = field(i)
        [In #f #o: ]
    }

    // debug()
    align(center)[= #cap(get(verbs, glob-i)) #thm1 for an arbitrary #obj1]
    align(center)[=== #authors(glob-i)]
    [\ ]
    par(first-line-indent: 2em)[
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

#nonsense[zenly;zastnsteinaiesrntoenz]
