# Overview

Note: This overview is very incomplete, and leaves out a lot of features whose structure is still being finalized. Additionally, everything listed here is subject to changes and revisions.

## Hello World

```scala
func main() do
    println("Hello World!")
end
```

## Variables

```scala
func main() do
    let foo = 10
    println(foo)
end
```

Lite _infers_ types of variables, so you don't have to provide explicit type annotations everywhere. By default, variables are immutable, but you can declare them as mutable with the `mut` keyword.

```scala
func main() do
    let mut foo = 10
    foo = 20
    println(foo)
end
```

Lite also has _arrays_, which are hold a bunch of values, each of which have to be of the same type.

```scala
func main() do
    let numbers = [1, 2, 3, 4, 5]
    println(numbers)
end
```

Lite also has _tuples_, which are collections of values which can be of different types.

```scala
func main() do
    let points = (10, -4)
end
```

Each tuple has its own type signature. The signature of `points` in the example above is `(int, int)`.

## Operators

Lite has all the expected logical operators, alongside the negation `!` and negative `-` infix operators.

## Control Flow

### If Expressions

```scala
if 5 == 5 do
    println("All is right with the world")
else do
    println("what the heck?")
end
```

Conditionals in Lite are much like what you'd see in other languages. One difference with languages like Python or Java is that conditionals in Lite, alongside many other language constructs, are _expressions_, and can evaluate to a value. Hence why they'd be called if _expressions_ instead of if _statements_. `if` will evaluate to the value of the last expression in whatever branch of the conditional is accepted. Here's an example.

```scala
func main() do
    let foo = 10;

    let bar = if foo == 10 do
        "is ten"
    else do
        "is not ten"
    end

    println(bar)  // "is ten"
end
```

All branches in an if expression must evaluate to the same data type.

### For loops

`for` loops in Lite are more like for loops in Python or Rust as opposed to C or Java.

```scala
func main() do
    for i in 1..10 do
        println(i)
    end
end
```

### While Loops

Lite loops are pretty much what you'd see in any other imperative language.

```scala
while 1 == 1 do
    println("all is right with the world")
end
```

## Functions

You can create functions in Lite using the `func` keyword.

```scala
func add(a: Int, b: Int) -> Int do
    a + b
end
```

When defining a function, any parameters you list need to be annotated with a type. If the function returns anything, it must also be annotated with the `-> Type` syntax. The `main()` function you'll see defined in binaries is the starting point of a Lite program.

The last expression in any function is what is returned. Alternatively, you can use an explicit `return` in cases where the previous option may not be applicable.

## Types

`type`s are the cornerstone of Lite's type system, they allow you to define things like sum types (which we'll call enums) and record types, as well as aliases to other types.

`type` has three forms. Enums, records, and aliases.

### Enums (or sum types)

You can define an enum with the following syntax

```scala
type Color = do
    Red or
    Green or
    Blue
end

func main() do
    let color = Color.Green
    println(color)
end
```

You can shorten the above declaration of `Color` to

```scala
type Color do
    Red or
    Green or
    Blue
end
```

pretty much removing the `=`.

Enums let you define a something which can be any variant of a set of values. Another powerful feature of enums is that you can associate some data with a variant. Here's an example.

```scala
type Message do
    Success or
    Failure(Str)
end

func main() do
    let failed_message = Message.Failure("Oh no, a problem occurred")
end
```

The data you can associate with an enum variants can also be much more structured, like a _record_.

### Records

You can define a record with the following syntax

```scala
type Person do
    name: Str and
    age: Int and
    job: Str
end

func main() do
    let joe = Person("joe", 36, "accountant")
    println(joe.name)
end
```

Records are a lot like structs in other language.

### Aliases

You can also use `type` to alias pre-existing types.

```scala
type MyInt = Int
```

### Option and Result

Two useful enums defined in the standard library are `Option` and `Result`.

### Option

Option is Lite's replacement for a `None`, `Nil`, or `Null` value. It lets you express that a value can either be *some*thing or nothing. It's defined as the following.

```scala
type Option[T] do
    Some(T) or
    None
end
```

You can use it in the following manner.

```scala
func main() do
    let an_int = Some(10);
    println(an_int);

    let no_value = None;
    println(no_value);
end
```

The reason you can directly access the `Some` and `None` variants of `Option` is because they are already imported in the _prelude_.

### Result

Result is Lite's take on error handling. You can use `Result` to represent a successful operation with some value attached, or an error. It's defined as the following.

```scala
type Result[T, E] do
    Ok(T) or
    Err(E)
end
```

## Pattern Matching

One important feature Lite has is _pattern matching_, through its `match` syntax. You can use pattern matching to compare some value against _patterns_, and run some code based on what pattern matches. Patterns can be something like a constant value, or something like a range or tuple. You can also destructure things like records and enums.

```scala
type Status do
    Finished or
    Pending or
    Preparing or
end

func main() do
    let status = Status.Ready

    match status do
        Status.Finished -> println("Operation has finished")
        Status.Pending -> println("Operation is pending")
        Status.Preparing -> println("Operation is preparing")
    end
end
```

You can also match against enums with variants that have values attached. We can display this with the build in `Option` enum.

```scala
type User do
    name: Str
end

func get_user(user_id: Int) -> Option[User] do
    let user = fetch_user_from_db(user_id)
    user
end

func main() do
    let user = get_user(1)

    match user do
        Ok(u) -> println("Hello {u.name}")
        None -> println("User does not exist")
    end
end
```

Like `if`, `for`, and `while`, Lite's `match` is also an expression. It evaluates to the value of whichever branch is chosen.

## Algebraic Effects

Lite has support for _algebraic effects_. Algebraic effects are a way for you to define some "effect" that a function may exhibit, and in turn handle them. You can draw a comparison with languages with `try/except`, like Python. The difference is that instead of catching exceptions, however, you can catch any kind of effect. Additionally, instead of stopping execution of the function, you can resume operation while also being able to give back some value to the function. Here's an example which defines the functionality of generators through effects.

```scala
effect Yield[T] do
    func yield(a: T)
end

func counter() -> Yield[Int] do
    let mut n = 0
    while true do
        yield(n)
        n += 1
    end
end

handle counter() do
    yield(a) -> do
        println(a)
        resume
    end
end
```

Effects in Lite are defined with the `effect` keyword, followed by its name and a list of operations under the effect. You can denote that a function exhibits some effects through its return type signature, as seen in the `-> Yield[Int]`. In the body, `yield` is called as any other function. You can then use the `handle` construct to handle effects. `handle` looks similar to `match`, and you can use syntax similar to pattern matching to handle multiple effects. The keyword `resume` is used whenever you want to resume execution of a function. You can also invoke `resume` with a value, which is sent back to the function.
