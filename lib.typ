#set text(size: 12pt)
#set page(margin: (x: 20%))

#let generation_symbol = (i, color: red) => {
    box(fill: color, inset: .3em, radius: 1pt, baseline: 30%)[#text(white)[#i]]
}

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
#let get(arr, i) = {arr.at(calc.rem(i, arr.len()))}
#let kv(dict, i) = {
    let k = dict.keys().at(calc.rem(i, dict.keys().len()))
    return (k, dict.at(k))
}

#let cap(str) = [#upper(str.at(0))#str.slice(1, str.len())]
#let sing(str) = {if str.at(0) in vowels [an #str] else [a #str]}


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
    "structural", "ontic", "ontic", "modal", "formal", "informal", "pseudo", 
    "natural", "enriched", "simplicial", "abelian", "constructable", "fixed", 
    "euclidean", "anti", "meta", "stochastically", "bijective", "semi"
)

#let fields = (
    "calculus", "statistics", "logic", "algebra", "set theory", "topology",
    "ontology","mereology"
)

#let glaze = (
    "ground breaking", "useful", "interesting", "highly relevant"
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
    "can be derived", "is divisible", "is an action", "repeats", "approximates the golden ratio", "is undefined", "is well ordered", "is a limit ordinal", "is a cardinal", "is natural", "is in a universe"
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
    return b1 + " " + b2 + " " + f
}

#let var = (i) => {
    let vars = ("x", "y", "μ", "Γ", "η", "α", "φ", "ο", "χ",
            "ε", "θ", "n", "i", "b", "z", "Κ", $W$, "r")

    let v = get(vars, i)

    if calc.rem(i, 3) == 0 {v = upper(v)}
    if calc.rem(i, 4) == 0 {v = $cal(#v)$}
    if calc.rem(i, 5) == 0 {v = $#v _(#calc.rem(i, 16))$}
    if calc.rem(i, 17) == 0 {v = $frak(#v)$}
    if calc.rem(i, 11) == 0 {v = $bb(#v)$}
    if calc.rem(i, 6) == 0 {v = $#v _(#get(vars, i + 3))$}
    if calc.rem(i, 7) == 0 {v = $#v ^(#get(vars, i * 2))$}

    return $#v$
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

#let eq-med = (i, heft: 1) => {
       // let f = get(funcs, i + n)
       // let (_, cv) = kv(connectives, i + n)
       // let g = get(funcs, i + 1 + n)
       let se = upper(get(alphabet, i))
       let v1 = var(i)
       let v2 = var(i + 1)
       let v3 = var(i + 2)
       let sub = eq-small(i)
       let sub2 = eq-small(i)
       let bo = get(binary_op, i)
    $
    #{for n in range(0, calc.floor(3.0 * heft)) {
       let rem = calc.rem(i + n, 18)
       [ #generation_symbol(rem, color: orange) ]
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
       else if rem == 16 [$v2^(sub)|$]
       else [$v1$]
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

    return "the" + " " + b + " " + a + " " + " " + o + " " + " " + k
}

#let fix(i) = {
    let obj = get(objects, i)
    let obj2 = get(objects, i+1)

    [Fix #obj2 in #obj]
}

#let nonsense(body) = {
    let count = counter("all")
    let section-types = ("lemma", "theorem", "definition")
    let chars = parse-actions(body).filter(char => char != none)
    if chars.len() == 0 { return }
    let glob-i = chars.map(c => to-int(c)).sum()
    let glob-thm1 = theorem(glob-i)
    let glob-thm2 = theorem(glob-i + 1)
    let glob-b = get(buzzwords, glob-i + 1)
    let glob-obj1 = get(objects, glob-i)
    let glob-obj2 = get(objects, glob-i + 1)
    let glob-obj3 = get(objects, glob-i + 2)
    let cases = 20;
    let references = calc.floor(chars.len() / 5) + calc.rem(glob-i, 3)
    let incomplete = text(red)[*incomplete*]

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
                    [#calc.rem(to-int(c), cases)])).join(" + ")]
                } else {
                    [#chars.slice(0, 4).map(c => point-pair([#to-int(c) mod #cases],
                    calc.rem(to-int(c), cases))).join(", "), ... 
                    #point-pair([#to-int(chars.last()) mod #cases],
                    [#calc.rem(to-int(chars.last()), 6)])]
                }
            }
        ]
    }


    let non-statement = (i, case) => {
        let action = get(("assume", "observe", "show", "extrapolate", "determine"), i);
        let awareness = get(("every student is aware", "it is easy to see", "one can easily see"), i);
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
        let stem = get(stems, i)
        let f = field(i)
        let g = get(glaze, i)
        let p = get(participles, i)
        let l = get(last_names, i)
        let reference = [\[#{1 + calc.rem(i, references)}\]] 

        [#generation_symbol(i) ]
        [#generation_symbol(case, color: blue) ]

        // plain text
        if case == 0 [Certain #ok\s in #f remain #stem\ed]
        else if case == 1 [Provided, #theorem(i) we have that: ]
        else if case== 2 [#cap(a) #sing(ok) is #stem\ed by #sing(b2) #ok2.]
        else if case == 3 [#cap(a) #q #sing(b3) #ok2, which #ck #sing(b) #ok. It #a2 #p: #eq-med(i)]
        else if case == 4 [#ok #p #stem\ing #theorem(i), which #awareness.]
        else if case == 5 [#cap(action) that #theorem(i) #ck #theorem(i+1) holds, as shown in #reference]
        else if case == 6 [#cap(a) we can #action #sing(ok) by #reference]
        else if case == 16 [The work of #get(last_names, i) on #theorem(i) is #g for #a2 #stem\ing #ok2 #p.]
        // Inline text
        else if case == 7 [By #stem\ing #sing(b) #ok on a #ok2, that is #eq-small(i) We reach #sing(b3) #b2 #ok3.]
        else if case == 8 [#sing(b2) #a #p, provided #eq-small(i)#eq-small(i + 1)]
        else if case == 9 [However, #eq-small(i).]
        else if case == 10 [#eq-small(i).]
        else if case == 11 [Of course #eq-small(i), provided #eq-small(i - 1).]
        else if case == 12 [#eq-small(i)]
        else if case == 13 [#cap(awareness) that #eq-small(i)]
        // medium
        else if case == 14 [#cap(action): #eq-med(i)]
        else if case == 15 [Most acedemics, provided $eq-med(i)$ would agree that #q #ok.]
        else if case == 17 [And as shown in #reference #eq-med]
        else if case == 18 [By #theorem(i) #eq-med(i)]
        // big equation
        else if case == 19 [#eq-med(i+1, heft: 2.2)]
        else [#eq-med(i, heft: 3.0)]
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
            let n = to-int(c)
            let case = calc.rem(i + n, cases) 

            if i == 0 { 
                count.step()
                context [#generation_symbol(count.get().first(), color:
                    green) ]
                [#non-introduction(glob-i)]
            } else {
                [#non-statement(n + glob-i, case)]
                if calc.rem(i, calc.rem(glob-i, 8) + 12) == 0 {
                    count.step()
                    count.step(level: 2)
                    context {
                        let level = count.get().first()
                        [ #generation_symbol(level, color: green)]
                        align(center)[
                            #level. 
                            #{
                                if level == 2 [MAIN RESULT]
                                else [#upper(theorem(i)) CASE]
                            }
                        ]
                    }
                }
            }
                context if (count.get().first() > 1 and calc.rem(n, 3) == 0) {
                    // abstract this into a method of some kind
                    [\ *#get(section-types, i) #count.display()*: #fix(i)]
                    count.step(level: 2)
                }
            [ ]
        }}
    ]

    let reference(i, case) = {
        case = calc.rem(case, 6)
        let title = {
            if case == 0 [_#theorem(i).split().map(it => cap(it)).join(" ")_]
            else if case == 1 [On the #cap(get(buzzwords, i)) #cap(get(objects, i))s]
            else if case == 2 [#theorem(i).split().map(it => cap(it)).join(" ") and Applications]
            else if case == 3 [On #field(i).split().map(it=> cap(it)).join(" ") and #get(objects, i)]
            else if case == 4 [#cap(get(buzzwords, i)) #field(i).split().map(it => cap(it)).join(" ")]
            else if case == 5 [#cap(get(buzzwords, i)) Methods in #field(i).split().map(it => cap(it)).join(" ")]
        }

        let publisher(i, case) = {

            let c = get(("American", "English", "Canadian", "Australian",
            "German", "French", "Italian", "Spanish", "Japanese", "Chinese",
            "Indian", "Russian", "Brazilian", "Mexican", "South African",
            "Korean", "Turkish", "Dutch", "Swedish", "Norwegian"), i);

                
            case = calc.rem(case, 20)
            if case < 4 [#c Mathematical Society]
            else if case < 8 [Society for #field]
            else if case == 8 [Cambridge University Press]
            else if case < 12 [Journal of #get(buzzwords, i) #get(objects, i+1)]
            else [#c Journal of mathematics]
        }

        let date(i) = {
            let month = get(("January", "February", "March", "April", "May",
            "June", "July", "August", "September", "October", "November",
            "December"), i)
            // between 1803 and 2028
            let year = 1803 + calc.rem(i, 225)
            [ #month #year]
        }

        if calc.rem(i, 7) == 0 {
            [#authors(i). #h(6pt) _#{title}_,  #authors(i * 9), #date(i)]
        } else {
            [#authors(i). #h(6pt) #title,  _#publisher(i * 7, i * 13)_, #date(i)]
        }
    }

    align(center)[REFERENCES]
    for i in range(0, references) [
         #par()[[#i] #h(4pt) #reference(i * glob-i, i + calc.rem(glob-i, 20))
        ] 
    ]
}

#nonsense[atetkses]
