The goal is to get the success rate of `slow/test` to 100%.

There is a corpus of more than 26 000 scala files. We check for the following correctness property:

```scala
val ast0 = parse(source0)
val source = prettyPrint(ast0)
val ast = parse(source)
ast0 == ast
```

Concreatly, this means you run `slow/test` and you hit `ctrl + c` on the first error. For example:

```
Formatting  15% │██           │  4078/26368 (0:00:07 / 0:00:39) Success: 74.40%
IdempotencyPropertyTest:91 [diff]: 
--- /home/gui/scala-syntax/tests/slow/target/repos/intellij-scala/testdata/typeInference/bugs5/SCL2426.scala
+++ /home/gui/scala-syntax/tests/slow/target/repos/intellij-scala/testdata/typeInference/bugs5/SCL2426.scala-formatted
@@ -1,6 +1,6 @@
 object Main {
   case class Cl[T](var x: T)
-  def withCl[T, R](v: Cl[T])(body: (Cl[T] => R) { def apply(v: Cl[T]): R }): R = body(v)
+  def withCl[T, R](v: Cl[T])(body: Cl[T] => R { def apply(v: Cl[T]): R }): R = body(v)
   withCl(Cl(10)) { (v: Cl[Int]) => {
     v.x = 20
     v.x
```

To fix this, we change the precedence of RefineType:

```
diff --git a/format/src/main/scala/org/scalafmt/internal/TreePrinter.scala b/format/src/main
index d441ad9..c734032 100644
--- a/format/src/main/scala/org/scalafmt/internal/TreePrinter.scala
+++ b/format/src/main/scala/org/scalafmt/internal/TreePrinter.scala
@@ -99,7 +99,7 @@ object TreePrinter {
           case t: Type.Refine =>
             val dtpe = t.tpe.fold(empty) { tpe =>
               val trailingSpace = if (t.stats.nonEmpty) space else empty
-              print(tpe) + trailingSpace
+              RefineTyp.wrap(tpe) + trailingSpace
```


If it's a regression (was working previously), it will show:


```
Regression: target/repos/intellij-scala/testdata/typeInference/bugs5/SCL2426.scala
```

And it will make the test fail.