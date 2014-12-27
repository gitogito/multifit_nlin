BEGIN {
    a = 2.34
    b = 0.123
    c = 0.5
    d = 0.7
    n = 10000
    for (i = 0; i <= n; i++) {
        x = (i - n/2) / n
        y = f(x)
        printf "%g\t%g\n", x, y
    }
}

function f(x,   y) {
    y = a*x^3 + b*x^2 + c*x + d
    r = (rand() - 0.5) / 10
    if (y + r < 0.0) r = 0
    y += r
    return y
}
