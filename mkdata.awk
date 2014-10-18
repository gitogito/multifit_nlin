BEGIN {
    na = 2.34
    m = 0.123
    s = 0.5
    n = 100
    for (i = 0; i <= n; i++) {
        x = 2 * i / n - 1
        y = f(x)
        printf "%g\t%g\n", x, y
    }
}

function f(x,   y, r) {
    y = na * exp(-(x - m)^2 / (2*s^2))
    r = (rand() - 0.5) / 10
    if (y + r < 0.0) r = 0
    y += r
    return y
}
