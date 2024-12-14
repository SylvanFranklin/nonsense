#let rec = (i) => {
    if i <= 0 {
        return "blast of"
    } else {
        return rec(i - 1)
    }
}
