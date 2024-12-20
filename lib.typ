#set text(size: 12pt)
#let entropy = state("entropy", 10) 
#let references = state("references", 0.0) 


#context {[#entropy.get()]}

#let generation_symbol = (i, color: red, body) => context {
    // Scale the color by the entropy 

    text(fill: color)[
        #if i != -1 and entropy.get() < 90 {
            let op = 100 - entropy.get()
            box(fill: color, inset: .3em, radius: 1pt, baseline:
            30%)[#text(white)[#i]]
        } #body
    ]
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

#let binary_op = (
    $times$, $+$, $-$, $|$, $in$, $<$, $equiv$, $emptyset.circle$, $~$,
    $diamond$, $arrow.squiggly$, $:=$, $subset$, $supset$, $ker$, $harpoon$, $mod$,  
    $supset$, $union.sq.big$, $hexa.stroked$, $alef$, $succ$, $fence.r$, $|->$,
    $<==>$, $triangle$, $psi$   
)
#let alphabet = "abcdefghijklmnopqrstuvwxyz"
#let to-int = (char) => {("ab*()&^%$#@!'cd:;efghijklmnopqrstuvwxyz").position(char)}
#let get(arr, i) = {arr.at(calc.rem(i, arr.len()))}

#let kv(dict, i) = {
    let k = dict.keys().at(calc.rem(i, dict.keys().len()))
    return (k, dict.at(k))
}

#let cap(str) = [#upper(str.at(0))#str.slice(1, str.len())]


#let sing(str) = {if lower(str.at(0)) in "aeiouy" {"an " + str} else {"a " + str}}

#let objects = (
    "functor", "monoid", "groupoid", "matrix", "group", "ring",
    "superset","category", "homoset", "co-monad", "endofunctor", "fibration",
    "morphism", "coequalizer", "category", "quiver", "bifunctor", "object",
    "sheaf", "torsor", "limit", "operad", "relation", "fusion", "subspace",
    "ordinal", "cardinal", "state", "number", "universe", "combinator", "vector",
    "scalar", "isomorphism", "manifold"
)

#let buzzwords = (
    "abstract", "relational", "substructural", "discrete", "inerpolated",
    "higher order", "paraconsistent", "interrelational", 
    "structural", "ontic", "modal", "formal", "pseudo",
    "natural", "enriched", "simplicial", "abelian", "constructable", "fixed",
    "non-euclidian", "bijective", "linear", "surjective",
    "uncountable", "differential", "ordered", "countable", "left-compact", 
    "connected", "hyprebolic", "anti-standard", "maximal"
)

#let fields = (
    "calculus", "logic", "algebra", "set theory", "topology", "ontology",
    "metrology", "measure theory", "combinatorics", "order theory", 
    "graph theory", "game theory", "geometry", 
)

#let glaze = (
    "ground breaking", "useful", "interesting", "highly relevant", "vital"
)

#let stems = (
    "enrich", "structur", "relat", "form", "inform", "interpolat", "construct",
    "generaliz", "abstract", "contain", "defin", "extract", "fix", "determin", 
)

#let last-names = (
    "Euler", "Bernstein", "Schröder", "Pascal", "Samuel", "Gödel", "Nozzle",
    "Cantor", "Jones", "Pythis", "Noether", "Rubble", "Russell", "Frege",
    "Zeno", "Curry", "Franklin", "Wager", "Pappas", "Fawkes", "Bacchus",
    "Lancaster", "Zilber", "Abou", "Snaggle", "Hitches", "Wumpin", "Quabosh"
);

#let connectives= (
    "implies": $==>$,
    "it follows that": $-->$,
    "if and only if": $<==>$,
    "is equivalent to": $equiv$,
    "does not imply": $arrow.double.not$,
    "is coextensive with": $union$, 
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

#let var(i) = {
    let vars = ("x", "y", "μ", "Γ", "η", "α", "φ", "ο", "χ",
            "ε", "θ", "n", "i", "b", "z", "Κ", $W$, "r")

    let v = get(vars, i)
    let case = calc.rem(i, 14) 
    if case == 0 {v = upper(v)}
    if case == 1 {v = $cal(#v)$}
    if case == 2 {v = $#v _(#calc.rem(i, 16))$}
    if case == 3 {v = $frak(#v)$}
    if case == 4 {v = $bb(#v)$}
    if case == 5 {v = $#v _(#get(vars, i + 3))$}
    if case == 6 {v = $#v ^(#get(vars, i * 2))$}
    return generation_symbol(-1, color: red)[$#v$]
}

#let func(i, case) = {
    let base = get(($sin$, $cos$, $log$, $sec$, $E$, $phi$, $"exp"$, $ln$, $sup$, $"efr"$, $zeta$, $det$, $tr$), i)
    case = calc.rem(case, 20)
    if case < 5 {base = $base^(-1)$}
    else if case < 8 {base = $base^2$}
    else if case == 9 {base = $base_2$}
    else if case == 10 {base = $overline(base)$}
    else if case == 11 {base = $underline(base)$}
    else if case == 12 {base = $hat(base)$}
    generation_symbol(case, color: aqua)[#base]
}

#let eq-small(i, n) = {
    let bo = get(binary_op, i*17+n*3)
    let v1 = var(i+n)
    let v2 = var((i*19)+1+n)
    let v3 = var((i*17)+3+n)
    let fun = func(i, i+1+n)
    let quan = get(($forall$, $exists$, $exists.not$, $!exists$), i*3+n)
    let case = calc.rem(i, 17)

    generation_symbol(case, color: black)[#{
        if case == 0 [$v1 v2 bo v3$] 
        else if case == 1 [$quan\(fun(v2) bo v1\)$] 
        else if case == 2 [$v3 bo v2$] 
        else if case == 3 [$fun(v2 / v1)$] 
        else if case == 4 [$quan \[v2 bo v3\]$] 
        else if case == 5 [$v3 fun\(v1\) v2$] 
        else if case == 6 [$v3^v2$] 
        else if case == 7 [$fun(v1 bo v3)$] 
        else if case == 8 [$v3 fun(2)$] 
        else if case == 9 [$case v1$] 
        else if case == 10 [$v1 bo v3$] 
        else if case == 11 [$v2 bo v3$] 
        else if case == 12 [$fun(func(#i, #i))$] 
        else if case == 13 [$v3 bo fun$] 
        else if case == 13 [$fun / v2$] 
        else if case == 13 [$abs(v3)$] 
        else if case == 14 [$v3_v2$] 
        else if case == 15 [$v1 bo v3$] 
    }]
}

#let eq-med = (i, heft: 3) => {
    $
    #{for n in range(0, calc.floor(heft)) {
       let se = upper(get(alphabet, i+n))
       let v1 = var(i+n)
       let v2 = var(i + 1+n)
       let v3 = var(i + 2+n)
       let sub = eq-small(i*67+n, n+i+7)
       let sub2 = eq-small(i+73+n, n+i+4)
       let bo = get(binary_op, i+n)
       let g = func(i*4, i+1+n)
       let rem = calc.rem(i + n*n*17, 18)

       generation_symbol(rem, color: orange)[#{
           if rem == 0 [$\{sub | (sub2) in bb(se)\}$]
           else if rem == 1 [$v1_v2 #g se$]
           else if rem == 2 [$v1 bo se subset {...v2^n}$]
           else if rem == 3 [$sub2/(sub bo v2)$]
           else if rem == 4 [$sub2 := abs(v1)v3$]
           else if rem == 5 [$(sub)_(sub2)$]
           else if rem == 6 [$integral_(i * n)^(v3)sub d v2$]
           else if rem == 7 [$(diff)/(v2 diff)$]
           else if rem == 8 [$lim_(v2 -> oo)(sub2)$]
           else if rem == 9 [$(sub)/(v2)$]
           else if rem == 10 [$(sub2)^(#g v3)$]
           else if rem == 11 [$cases(v1\:sub=v2,v3\:rem=#g)$]
           else if rem == 12 [$(sum_(sub2)^(v2))/v3$]
           else if rem == 13 [$v3$]
           else if rem == 14 [$sub$]
           else if rem == 15 [$sub2$]
           else if rem == 16 [$v2^(sub)|$]
           else [$v1$]
        }]

    }}
    $
}


#let authors = (i) => {
    range(0, calc.rem(i, 4) + 1).map(n => 
        [#generation_symbol(n, color: gray)[#upper(get(alphabet, i + n))]. #generation_symbol(i+n, color: orange)[#get(last-names, i + n)]]
    ).join(", ")
}

#let theorem = (i) => {
    let o = get(objects, i)
    let b = get(buzzwords, i)
    let a = if calc.rem(i, 2) == 0 {
        get(last-names, i)
    } else {
        get(buzzwords, i - 2)
    }

    let k = get(
        ("lemma", "theorem", "axiom", "conjecture", "principle",
        "extension", "theory"), i
    )

    return "the" + " " + b + " " + a + " " + " " + o + " " + " " + k
}

#let property(i, case) = {
    let case = calc.rem(case, 5)
    generation_symbol(case, color: olive)[#{
        if case == 0 [approximates #var(i)]
        else if case == 1 [is #sing(get(objects, i))]
        else if case == 2 [#get(("commutes", "permutes", "repeats", "tiles the plane"), i)]
        else if case == 3 [is #get(("undefined", "in a universe", "well defined", "even", "a basis"), i)]
        else [is #get((buzzwords), i + 3)]
    }]
}

#let non-setup(i, case) = {
    let case = calc.rem(case, 5)
    let buzz1 = get(buzzwords, i+1)
    let buzz2 = get(buzzwords, i+2)
    let buzz3 = get(buzzwords, i+3)
    let obj1 = get(objects, i+88)
    let obj2 = get(objects, i+19+1)
    let eq1 = eq-small(case + 1, i+1)
    let eq2 = eq-small(case + 3, i * 13 + 1)
    let prop = property(case * 3, i+1)
    let (name, symbol) = kv(connectives, i*7)

    generation_symbol(case, color: navy)[#{
        if case == 0 [Fix #sing(buzz1) #buzz2 #obj1, #eq1 in #sing(buzz3) #obj2, #eq2.]
        else if case == 1 [Let #sing(buzz2) #obj1 that #prop, be defined #eq2]
        else if case == 2 [Fix #sing(buzz1) #obj1 in #sing(buzz2) #buzz3 #obj2.]
        else if case == 3 [Assume #sing(buzz1) #sing(buzz2) #obj1 #prop.]
        else [Assume #eq1 #symbol #eq2.]
    }]
}
#let non-statement = (i, case) => {
        let action = get(("assume", "observe", "show", "determine"), i);
        let awareness = get(("it is obvious", "it is easy to see", "one can easily see", "of course"), i);
        let person = generation_symbol(calc.rem(i*3, 6))[#get(("self respecting", "well educated", "logical", "sane", "competent"), i*3)]

        person += " " + get(("student", "mathematician", "logician", "author"), i)

        let obj1 = get(objects, i * 7 + 9)
        let obj2 = get(objects, i * 8 + 7) 
        let quanp = get(("for all", "for every", "each", "every"), i)
        let quans = get(("there exists", "there does not exist",
        "there exists a unique"), i)
        let obj3 = get(objects, i * 9 + 8) 
        let buzz1 = get(buzzwords, i)
        let buzz2 = get(buzzwords, i - 1)
        let buzz3 = get(buzzwords, i - 2)
        let (ck, cv) = kv(connectives, i)
        ck = generation_symbol(case, color: purple)[#ck]
        cv = generation_symbol(case, color: purple)[#cv]
        let adverb1 = get(adverbs, i)
        let adverb2 = get(adverbs, i+1)
        let stem = generation_symbol(case, color: olive)[#get(stems, i)]
        let field = field(i)
        let glaze = generation_symbol(i)[#get(glaze, i)]
        let property = property(i, i * case * 799)
        let last-name = generation_symbol(calc.rem(i, last-names.len()))[#get(last-names, i)]
        let res = theorem(i*3)
        context {
        let reference = generation_symbol(-1, color: orange)[[#{1 + calc.rem(i * case * 979, references.get())}]] 

        generation_symbol(case, color: blue)[
        #{
            if case == 0 [Certain #obj1\s in #field remain #stem\ed, under the assumtption that #res holds for all #obj2\s.]
            else if case == 1 [Provided, #res we have that: ]
            else if case== 2 [#cap(adverb1) #sing(obj1) is #stem\ed by #sing(buzz2) #obj2.]
            else if case == 3 [#cap(adverb1) there exists #sing(buzz3) #obj2, which #ck #sing(buzz3) #obj1. It #adverb2 #property: #eq-med(i)]
            else if case == 4 [#cap(sing(obj1)) #property if it is #sing(buzz3) #obj1, which #awareness.]
            else if case == 5 [Since #res #property #quans #sing(obj1), as shown in #reference]
            else if case == 6 [#cap(adverb1) we can #action #sing(obj1) by #reference]
            else if case == 14 [#cap(action): #eq-small(case, i*5)]
            else if case == 16 [The work of #last-name on #res #reference is #glaze for #adverb2 #stem\ing #sing(buzz1) #obj1, which #adverb1 #property.]
            // Inline text
            else if case == 7 [By #stem\ing #sing(buzz1) #obj1 on a #obj2, that is #eq-small(case+i, i+case*3) We reach #sing(buzz3) *#buzz2* #obj3.]
            else if case == 8 [#cap(sing(buzz2)) #adverb1 #property, provided #eq-small(i, i*case+3)#eq-small(i + 1, case*i*83)]
            else if case == 9 [However, #eq-small(i, case*i*3), as shown #adverb2 in #reference is #sing(buzz2) #obj3 and #property.]
            else if case == 10 [#eq-small(i+case, case*17).]
            else if case == 11 [#cap(awareness) #reference #eq-small(i, case*i+1), provided #eq-small(i - 1, case+19).]
            else if case == 12 [#eq-small(i, case * 9)]
            else if case == 13 [#cap(awareness) that #eq-small(i*17, case+i)]
            // medium
            else if case == 15 [Provided #eq-small(i, i*case*3), as shown in #reference any #person would agree that every #obj1 #property]
            else if case == 17 [And as shown in #reference, by #last-name #eq-med(i, heft:2)]
            else if case == 18 [By #res #eq-med(i, heft: 2)]
            // big equation
            else if case == 19 [#eq-med(i+1, heft: 3)]
            else [Breaking into the two cases:  #grid(gutter: 6em, columns: (1fr, 1fr), [#align(center)[Case I] #eq-med(i+3, heft: 3)], [#align(center)[Case II] #eq-med(i, heft:1)])]
                }
            ]
        }
}

#let non-proof(i, case) = {
    let case = calc.rem(case, 5)
    par[
        #generation_symbol(case, color: fuchsia)[
        _proof_ #non-setup(i, case+1) #non-setup(i+1, case - 1).
        It therefore follows
        #v(0.2em)#h(1fr)$square.big$]
    ]
    
}

#let proof-theorem-lemma(i, heading) = {
    let case = calc.rem(i, 10)
    let var = var(i)
    let buzz = get(buzzwords, i*59+1)
    let obj = get(objects, i + 1)
    let prop = property(i+1, case * 3)
    let prop2 = property(i, case * 3 * i + 3)
    let qed = [#v(0.2em)#h(1fr)$square.big$]
    let setup = non-setup(case, i)
    let setup2 = non-setup(case+1, i+1)
    let pf = non-proof(i, case+1)
    
    generation_symbol(case, color: purple)[#{
        if case == 0 [#strong[Theorem #heading] #setup2 #setup #pf]
        else if case == 1 [#strong[Proposition #heading] #setup #pf]
        else if case == 2 [#strong[Definition #heading] Let #eq-small(i*5, case * 13) be #buzz. #cap(sing(obj)) #prop if it #prop2. #pf]
        else if case == 3 [#strong[Definition #heading] #setup2 #pf]
        else if case == 4 [#strong[Definition #heading] #setup #setup2 #pf]
        else if case == 5 [#strong[Lemma #heading] #math.italic([#setup2 #setup])]
        else if case == 5 [#strong[Lemma #heading] #emph([#setup1])]
        else if case == 6 [#strong[Theorem #heading] #emph([#setup #setup2]). #pf]
        else [#strong[Lemma #heading] #text(weight: "regular", style: "italic", [Let #eq-small(i+3, case * 79) be #buzz, then #eq-small(i+5, case * 97).]) #pf]
    }]
} 
 
#let nonsense(body) = {
    let count = counter("all")
    let chars = parse-actions(body).filter(char => char != none)
    if chars.len() == 0 { return }
    let glob-i = chars.map(c => to-int(c)).sum()
    let glob-thm1 = theorem(glob-i)
    let glob-thm2 = theorem(glob-i + 1)
    let glob-b = get(buzzwords, glob-i + 1)
    let glob-obj1 = get(objects, glob-i)
    let glob-obj2 = get(objects, glob-i + 1)
    let glob-obj3 = get(objects, glob-i + 2)
    let cases = 21;

    context {
        // entropy.update(val => 0)
        references.update(val => calc.floor(chars.len() / 5) + calc.rem(glob-i, 3))
    }

    let reference(i, case) = {
        case = calc.rem(case, 6)
        let title = generation_symbol(case, color: eastern)[#{
            if case == 0 [_#theorem(i).split().map(it => cap(it)).join(" ")_]
            else if case == 1 [On the #cap(get(buzzwords, i)) #cap(get(objects, i))s]
            else if case == 2 [#theorem(i).split().map(it => cap(it)).join(" ") and Applications]
            else if case == 3 [On #field(i).split().map(it=> cap(it)).join(" ") and #get(objects, i)]
            else if case == 4 [#cap(get(buzzwords, i)) #field(i).split().map(it => cap(it)).join(" ")]
            else if case == 5 [#cap(get(buzzwords, i)) Methods in #field(i).split().map(it => cap(it)).join(" ")]
        }]

        let publisher(i, case) = {

            let c = get(("American", "English", "Canadian", "Australian",
            "German", "French", "Italian", "Spanish", "Japanese", "Chinese",
            "Indian", "Russian", "Brazilian", "Mexican", "South African",
            "Korean", "Turkish", "Dutch", "Swedish", "Norwegian"), i);

            generation_symbol(case, color: olive)[#{
                case = calc.rem(case, 20)
                if case < 4 [#c Mathematical Society]
                else if case < 8 [Society for #field(i)]
                else if case == 8 [Cambridge University Press]
                else if case < 12 [Journal of #get(buzzwords, i) #get(objects, i+1)]
                else [#c Journal of mathematics]
            }]
        }

        let date(i) = {
            let month = get(("January", "February", "March", "April", "May",
            "June", "July", "August", "September", "October", "November",
            "December"), i)
            // between 1803 and 2028
            let year = 1803 + calc.rem(i, 225)
            generation_symbol(calc.rem(i, 12), color: gray)[#month #year]
        }

        if calc.rem(i, 7) == 0 {
            [#authors(i+case*5). #h(6pt) _#{title}_,  #authors(i * 9), #date(i)]
        } else {
            [#authors(i+7*case+4). #h(6pt) #title,  _#publisher(i * 7, i * 13)_, #date(i)]
        }
    }

    let debug = () => {
        let point-pair = (a, b) => $vec(delim: "[", #a, #text(blue)[#b])$
        block(inset: 1em, stroke: 0.1em, radius: 1em, width: 100%)[
            *entropy: * #entropy \ \ 
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
        = #generation_symbol(1, color: green)[#cap(get(stems, glob-i))ing #glob-thm1 for #sing(glob-b) #glob-obj2]
        #v(1em) #authors(glob-i) #v(2em)
    ]

    align(center)[1. INTRODUCTION]
    par(hanging-indent: -2em, justify: true)[
        #{for (i, c) in chars.enumerate() {
            let n = to-int(c)
            let case = calc.rem(i + n + 1, cases) 
            if i == 0 { 
                count.step()
                context generation_symbol(count.get().first(), color:
                    green)[#non-introduction(glob-i)]

            } else {
                [#non-statement(n + glob-i, case)]
                if calc.rem(i, calc.rem(glob-i, 8) + 12) == 0 {
                    count.step()
                    count.step(level: 2)
                    context {
                        let level = count.get().first()
                        generation_symbol(level, color: green)[
                        #align(center)[
                            #level. 
                            #{
                                if level == 2 [MAIN RESULT]
                                else [#upper(theorem(i)) CASE]
                            }
                        ]]
                    }
                }
            }
                context if (count.get().first() > 1 and calc.rem(n, 3) == 0) {
                    // abstract this into a method of some kind
                    [\ \ #proof-theorem-lemma(i* 3, count.display())]
                    count.step(level: 2)
                }
            [ ]
        }}
    ]

    align(center)[REFERENCES]
    context for i in range(0, references.get()) [
         #par()[[#i] #h(4pt) #reference(i * glob-i, i + calc.rem(glob-i, 20))
        ] 
    ]
}

#nonsense[nkaeskaosesltuamskteslaoskseslteskseruaorstenst]
