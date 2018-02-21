The goal is to get the success rate of `slow/test` to 100%.

There is a corpus of more than 26 000 scala files. We check for the following correctness property:

```scala
val ast = parse(source)
val source' = prettyPrint(ast)
val ast' = parse(source')
ast == ast'
```

Concreatly, this means you run `slow/test` and you hit `ctrl + c` on the first error. For example:

