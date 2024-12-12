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

#let vars = ("x", "y", "z", $theta$, $lambda$)
#let funcs = ($sin$, $cos$, $arccos$, $log$, $arctan$, $E$, $phi$)
#let joiner = ($and$, $or$, $xor$)
#let alphabet = "abcdefghijklmnopqrstuvwxyz"
#let vowels = "aeiouy"
#let to-int = (char) => {("ab*()&^%$#@!'cd:;efghijklmnopqrstuvwxyz").position(char)}
#let get = (arr, i) => {arr.at(calc.rem(i, arr.len()))}
#let kv = (dict, i) => {
    let k = dict.keys().at(calc.rem(i, dict.keys().len()))
    return (k, dict.at(k))
}
#let cap = (str) => [#upper(str.at(0))#str.slice(1, str.len())]
#let sing = (str) => {if str.at(0) in vowels [an #str] else [a #str]}

#let objects = (
    "functor", "transformation", "monoid", "groupoid", "topos", 
    "closed category", "homoset", "comonad", "endofunctor", "fibration",
    "lateral morphism", "coequalizer", "category", "quiver", "bifunctor",
    "object", "sheaf", "torsor", "limit", "operad", "part-whole relation",
    "fusion", "subspace", "ordinal", "cardinal", "state",
    "hyperreal number", "universe", "combinator", "space"
)

#let symbols = (
    "metacyclic integral": $integral.cont.ccw$,
    combinator: $lambda Epsilon_1$ ,
    "functoral cardial hackset": $f circle.small g$,
    section: $section$,
    "oblique vector": $vec(cal(m), cal(Z))$ 
)

#let buzzwords = (
    "abstract", "relational", "substructural", "discrete", "inerpolated",
    "intuitional", "higher order", "paraconsistent", "interrelational",
    "structural", "ontic", "semi ontic", "modal", "formal", "informal", "psuedo", 
    "natural", "enriched", "simplicial", "abelian", "constructable", "fixed", 
    "euclidian", "anti", "meta", "stochastically", "bijective"
)

#let fields = (
    "calculus", "statistics", "logic", "algebra", "set theory", "topology",
    "ontology","mereology"
)

#let stems = (
    "enrich", "structur", "relat", "form", "inform", "interpolat", "construct",
    "generaliz", "abstract", "contain", "defin", "extract", "fix", "determin", 
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

#let participles = (
    "commutes", "permutes", "tiles the plane", "is a monad", "is a functor",
    "can be derived", "is divisible", "is an action", "repeates", "approximates the golden ratio", "is undefined", "is well ordered", "is a limit ordinal", "is a cardinal", "is natural", "is in a universe"
);


#let connectives= (
    "implies": $==>$,
    "it follows that": $-->$,
    "only if": $<==>$,
    "is equivalent to": $equiv$,
    "does not imply": $equiv$,
    "is coextensive with": $union$, 
)


#let quantifiers = (
    "for all": $forall$,
    "there exists": $exists$,
    "there does not exist": $exists.not$,
    "there exists a unique": $!exists$ 
)

#let adverbs = (
    "vacuously", "trivially", "logically", "necessarily", "formally",
    "ostensibly","hypothetically", "obliquely", "indirectly",
    "superficially", "redundantly", "strictly", "presumably", "nominally",
    "fundamentally",
)


#let field = (i) => {
    let b1 = get(buzzwords, i)
    let b2 = get(buzzwords, i + 2)
    let f = get(fields, i)
    [#b1 #b2 #f]
}

#let eq = (i, heft: 3) => {

    $
    #{for n in range(0, heft) {
       let rem = calc.rem(i + n, 10)
       let v1 = get(alphabet, i + n)
       let v2 = get(alphabet, i + n - 3)
       let v3 = get(alphabet, i + n - 2)
       let f = get(funcs, i + n)
       let (_, cv) = kv(connectives, i + n)
       let g = get(funcs, i + 1 + n)

       if rem == 0 [$#f$]
       else if rem == 1 [$(v2 in v3)$]
       else if rem == 2 [${#f sinh v2}^(cal(v2))$]
       else if rem == 3 [$f oo g$]
       else if rem == 4 [$sum_v1^v3$]
       else if rem == 5 [$product_v2^cal(v2 - v1)$]
       else if rem == 6 [$(sum_()^()$]
       else if rem == 7 [$-$]
       else if rem == 8 [$times$]
       else if rem == 9 [$==>$]
       else if rem == 10 [$<==>$]
       else [$$]
    }}
    $

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
        "theory"), i
    )

    [the #b #a #o #k]
}

#let nonsense(body) = {
    let chars = parse-actions(body).filter(char => char != none)
    if chars.len() == 0 { return }
    let glob-i = chars.map(c => to-int(c)).sum()
    let glob-thm1 = theorem(glob-i)
    let glob-thm2 = theorem(glob-i + 1)
    let glob-b = get(buzzwords, glob-i + 1)
    let glob-obj1 = get(objects, glob-i)
    let glob-obj2 = get(objects, glob-i + 1)
    let glob-obj3 = get(objects, glob-i + 2)

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
        let action = get(("Assume", "Observe", "By showing"), i);
        let (ok, ov) = kv(symbols, i)
        let (ck, cv) = kv(connectives, i)
        let (ok2, ov2) = kv(symbols, i + 2)
        let (ok3, ov3) = kv(symbols, i + 5)
        let q = get(quantifiers.keys(), i)
        let b = get(buzzwords, i)
        let b2 = get(buzzwords, i - 1)
        let b3 = get(buzzwords, i - 2)
        let a = get(adverbs, i)
        let a2 = get(adverbs, i+1)
        let v = get(stems, i)
        let f = field(i)
        let p = get(participles, i)
        let l = get(last_names, i)
        
        let case = calc.rem(i, 5) 
        if case == 0 {
            [By #v\ing #sing(b) #ok on a #ok2, that is #eq(i) We reach #sing(b3) #b2 #ok3.]
        } else if case == 1 {
            [#action: #eq(i, heft: 12) ]
        } else if case == 2 {
            [#a #sing(ok) is #v\ed by #sing(b2) #ok2.]
        } else if case == 3 {
            [#cap(a) #q #sing(b3) #ok2, which #ck #sing(b) #ok. It #a2 #p: #eq(i)]
        } else {
            [On the other hand, #v\ing #sing(b) #glob-obj1, #a2 creates #sing(b2) #ov2.]
        }
    }

    let non-introduction = (i) => {
        let casual = (
            "extremely", "easily", "widely"
        )

        let c = get(casual, i)
        let obj = get(objects, i + 1)
        let f = field(i+1)
        let s = get(stems, i)

        [In #f #glob-thm1 for #sing(glob-b) #obj is #c #s\able.]
    }

    // debug()
    align(center)[
    = #cap(get(stems, glob-i))ing #glob-thm1 for #sing(glob-b) #glob-obj2
    ==== #authors(glob-i) 
    \
    ]
    align(center)[1. Introduction]
    par(hanging-indent: -2em, justify: true)[
        #{for (i, c) in chars.enumerate() {
            let n = to-int(c)
            if i == 0 {
                [#non-introduction(glob-i)]
            } else {
                [#non-statement(n)]
            }
            [ ]
        }}
    ]
}

#nonsense[andkaezazll]
