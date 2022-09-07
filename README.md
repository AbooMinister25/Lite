# The Lite Programming Language

Lite is a general purpose and easy to learn programming language. Lite aims to be both easy to read and write, while also enabling the development of fast and efficient programs.

Lite is still under heavy development, and has not yet reached a working stage.

Here's a taste of the eventual syntax

```ocaml
func fib(n: Int) -> Int do
    if n < 2 do
        return n
    end

    fib(n - 1) + fib(n - 2)
end

func main() do
    let fib_of_10 = fib(10)
    println(fib_of_10)
end
```

You can look at [OVERVIEW.md](OVERVIEW..md) for a more in-depth overview of the language features.