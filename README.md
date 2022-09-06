# The Lite Programming Language

Lite is a general purpose and easy to learn programming language. Lite aims to be both easy to read and write, while also enabling the development of fast and efficient programs.

Lite is still in development, and is undergoing multiple rewrites :p

Here's a taste of the eventual syntax

```scala
func fib(n: int) do
    if n < 2 do
        return n
    end

    fib(n - 1) + fib(n - 2)
end

func main() do
    let fib_of_10 = fib(10)
    print(fib_of_10)
end
```
