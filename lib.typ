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
    "structural", "ontic", "ontic", "modal", "formal", "informal", "psuedo", 
    "natural", "enriched", "simplicial", "abelian", "constructable", "fixed", 
    "euclidian", "anti", "meta", "stochastically", "bijective", "semi"
)

#let fields = (
    "calculus", "statistics", "logic", "algebra", "set theory", "topology",
    "ontology","mereology"
)

#let stems = (
    "enrich", "structur", "relat", "form", "inform", "interpolat", "construct",
    "generaliz", "abstract", "contain", "defin", "extract", "fix", "determin", 
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


#let binary_op = (
    $times$, $+$, $-$, $|$, $in$, $=$, $<$, $<=$, $>=$, $equiv$, $<==>$,
    $diamond$, $arrow.squiggly$  
)

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

#let var = (i) => {
    let v = get(
    ("x", "y", "μ", "Γ", "η", "α", "φ", "ο", "χ",
    "ε", "θ", "n", "i", "b", "z", "Κ", $W$ 

    ), i)
    if calc.rem(i, 3) == 0 {v = upper(v)}
    if calc.rem(i, 4) == 0 {v = $cal(v)$}
    if calc.rem(i, 17) == 0 {v = $frak(v)$}
    if calc.rem(i, 11) == 0 {v = $bb(v)$}

    return $v$
}

#let eq-small = (i, heft: 3) => {
    let bo = get(binary_op, i)
    let v1 = var(i)
    let v2 = var(i+1)
    let v3 = var(i+3)
    let fun = get(funcs, i)
    if calc.rem(i, 6) == 0 [$v1 v2 bo v2$] 
    else if calc.rem(i, 6) == 1 [$v1 v2$] 
    else if calc.rem(i, 6) == 2 [$v3 bo v2$] 
    else if calc.rem(i, 6) == 3 [$fun\(v2\)$] 
    else if calc.rem(i, 6) == 4 [$v3 bo v2$] 
    else if calc.rem(i, 6) == 5 [$v3 fun\(v1\) v2$] 
}



#let eq-med = (i) => {
    $
    #{for n in range(0, 3) {
       let rem = calc.rem(i + n, 18)
       // let f = get(funcs, i + n)
       // let (_, cv) = kv(connectives, i + n)
       // let g = get(funcs, i + 1 + n)
       let se = upper(get(alphabet, n))
       let v1 = var(i)
       let v2 = var(i + 1)
       let v3 = var(i + 2)
       let sub = eq-small(i + n)
       let sub2 = eq-small(i + n + 1)
       let bo = get(binary_op, n * i)

       if rem == 0 [$\{sub | (sub2) in bb(se)\}$]
       else if rem == 1 [$v1_v2 ker se$]
       else if rem == 2 [$v1 bo se subset {...v2^n}$]
       else if rem == 3 [$v3 harpoon (sub2)$]
       else if rem == 4 [$sub2 := v2$]
       else if rem == 5 [$sum_(sub2)^(v2)$]
       else if rem == 6 [$integral_(i * n)^(v3)sub d v2$]
       else if rem == 7 [$(diff)/(v2 diff)$]
       else if rem == 8 [$lim_(v2 -> oo)(sub2)$]
       else if rem == 9 [$(sub)/(v2)$]
       else if rem == 10 [$(sub)^(sub2)$]
       else if rem == 11 [$(sub)_(sub2)$]
       else if rem == 12 [$v2$]
       else if rem == 13 [$v3$]
       else if rem == 14 [$sub$]
       else if rem == 15 [$sub2$]
       else if rem == 16 [$bo$]
       else [$v1$]
    }}
    $
}

#let eq-large = (i) => {
    if calc.rem(i, 4) == 0 {$ (#eq-med(i))/(#eq-med(i+1)) $} 
    if calc.rem(i, 4) == 1 {$ lr(#eq-med(i)|) --> #eq-med(i + 1) $} 
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
    let count = counter("all")
    let section-types = ("lemma", "theorem", "defenition")
    let chars = parse-actions(body).filter(char => char != none)
    if chars.len() == 0 { return }
    let glob-i = chars.map(c => to-int(c)).sum()
    let glob-thm1 = theorem(glob-i)
    let glob-thm2 = theorem(glob-i + 1)
    let glob-b = get(buzzwords, glob-i + 1)
    let glob-obj1 = get(objects, glob-i)
    let glob-obj2 = get(objects, glob-i + 1)
    let glob-obj3 = get(objects, glob-i + 2)
    let cases = 7;

    let debug = () => {
        let point-pair = (a, b) => $vec(delim: "[", #a, #text(blue)[#b])$
        block(inset: 1em, stroke: 0.1em, radius: 1em, width: 100%)[
            *seed* : #{
                if chars.len() < 6 {
                    [#chars.map(c => point-pair(c, to-int(c))).join(" + ") =
                     #glob-i - #text(red)[*global seed*]]
                } else {
                    [#chars.slice(0, 4).map(c => point-pair(c,
                    to-int(c))).join(" + ") + ... + #point-pair(chars.last(),
                    to-int(chars.last())) = - #glob-i  #text(red)[*global seed*]]
                }
            }
            \
            \
            *sentences* : #{
                if chars.len() < 6 {
                    [#chars.map(c => point-pair([#to-int(c) mod #cases],
                    [#calc.rem(to-int(c), 6)])).join(" + ")]
                } else {
                    [#chars.slice(0, 4).map(c => point-pair([#to-int(c) mod #cases],
                    calc.rem(to-int(c), 6))).join(", "), ... 
                    #point-pair([#to-int(chars.last()) mod #cases],
                    [#calc.rem(to-int(chars.last()), 6)])]
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
        let case = calc.rem(i, cases) 

        if case == 0 [By #v\ing #sing(b) #ok on a #ok2, that is #eq-small(i) We reach #sing(b3) #b2 #ok3.]
        else if case == 1 [#cap(action): #eq-med(i) ]
        else if case == 2 [#cap(a) #sing(ok) is #v\ed by #sing(b2) #ok2.]
        else if case == 3 [#cap(a) #q #sing(b3) #ok2, which #ck #sing(b) #ok. It #a2 #p: #eq-large(i)]
        else if case == 4 [Everything by #p #v\ing #theorem(i).]
        else if case == 5 [Most acedemics, provided $eq-med(i)$ would agree that #q #ok.]
        else if case == 6 [Trivially, #eq-large(i)]
        else [On the other hand, #v\ing #sing(b) #glob-obj1, #a2 creates #sing(b2) #ov2.]
    }

    let non-introduction = (i) => {
        let casual = (
            "extremely", "easily", "widely", "long pursued"
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
        #v(1em) #authors(glob-i) #v(2em)
    ]


    

    align(center)[1. INTRODUCTION]
    par(hanging-indent: -2em, justify: true)[
        #{for (i, c) in chars.enumerate() {
            let n = to-int(c) + i
            if i == 0 { 
                count.step()
                [#non-introduction(glob-i)]
            } else if calc.rem(i, 3) == 0 and calc.rem(n, 2) == 0 {
                count.step(level: 2)
                [\ *#get(section-types, n) 1.2*]
            }
            else [#non-statement(n)]
            [ ]
        }}
    ]
}

#nonsense[asozarsztoarseoarsiularstneioaosetnarosietnorsieanit]
