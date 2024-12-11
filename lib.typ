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
    "fusion", "subspace", "ordinal", "cardinal", 
    "hyperreal number", "universe", "combinator"
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
#let verbs = (
    "generalizing", "abstracting", "containing",
    "defining", "constructing", "extracting", "fixing"
)

#let openers = (
    "a problem arises", "a long sought after result", "a common challenge may be")

#let field = (i) => {
    let b1 = get(buzzwords, i)
    let b2 = get(buzzwords, i + 2)
    let f = get(fields, i)
    [#b1 #b2 #f]
}

#let eq = (i) => {
   let rem = calc.rem(i, 7)
   let v1 = get(alphabet, i)
   let v2 = get(alphabet, i - 3)
   let v3 = get(alphabet, i - 2)
   let f = get(funcs, i)
   let (_, cv) = kv(connectives, i)
   let f2 = get(funcs, i+1)

   if rem == 0 [$ integral_(cal(v1))^(v2) #f_(v1) () divides v1^(f2 + v3) diff$]
   else if rem == 1 [$ (v3 + #f\(cal(v2)\))/infinity = i^4^(#f2\(v1\)) $]
   else if rem == 2 [$ #v2 times mat(i, 0; -i, i^2) $]
   else if rem == 3 [$ sum_0^(v3 = cal(v1))Phi(3/4)$]
   else if rem == 4 [$ f\(f2\(#v2 + #v1\)\) #c #f2 "finite"$ ]
   else if rem == 5 [$ "(x|s|r" #v2 cal(e))^f\(#i\)$]
   else if rem == 6 [$ v2 in {u = #f(v_3) | psi^(i^(#i)) : v_3 in KK} $]
   else if rem == 7 [$ v3 - cal(v1) + (f\(m\))/n J $]
   else []
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
        let (ok, ov) = kv(symbols, i)
        let (ok2, ov2) = kv(symbols, i + 2)
        let q = get(quantifiers.keys(), i)
        let b = get(buzzwords, i)
        let b2 = get(buzzwords, i - 1)
        let b3 = get(buzzwords, i - 2)
        let a = get(adverbs, i)
        let v = get(verbs, i)
        let (ck, cv) = kv(connectives, i)
        let f = field(i)
        let l = get(last_names, i)
        
        let case = calc.rem(i, 5) 
        if case == 0 {
            [By #v #sing(b) #ok on a #ok2, that is $ov$.]
        } else if case == 1 {
            [Assume: #eq(i).]
        } else if case == 2 {
            [First, #a #v #sing(b2) #obj2, such that: ]
        } else if case == 3 {
            [#cap(a) #q #sing(b3) #obj3, #ck #sing(b) #obj1 #() ie #eq(i)]
        } else {
            [On the other hand, #a #v #sing(b) #obj1, #a #v #sing(b2) #obj2.]
        }
    }

    let non-introduction = (i) => {
        let o = get(openers, i)
        let v = get(verbs, i)
        let f = field(i)
        [In #f #o #v ]
    }

    // debug()
    align(center)[
    = #cap(get(verbs, glob-i)) #thm1 for an arbitrary #obj1
    ==== #authors(glob-i) 
    \
    ]
    align(center)[1. Introduction]
    par(hanging-indent: -2em, justify: true)[
        #{for (i, c) in chars.enumerate() {
            let n = to-int(c)
            if i == 0 {
                [#non-statement(n)]
            } else {
                [#non-statement(n)]
            }
            [ ]
        }}
    ]
}

#nonsense[zlm]
