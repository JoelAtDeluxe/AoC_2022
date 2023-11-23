# Advent of Code 2022 -- In Scala

Level: Some Scala work, some functional-ish programming experience
Goal: Pretty much entirely functinoal approach. allowing myself these things:

1. Overwriting containers:

   ```scala
   var l = List(1,2,3)
   l = l :+ 4
   ```
2. For loops and mutability in debug code / during samples (primarily for speed and verifying correctness)
3. After solve, a mutable version if I think the (mostly) immutable version is too slow

## Language Learnings / Examples for when I forget this later

* Metals in VSCode (Day 0)
  * Create a build.sbt file (can be _completely_ empty)
  * have a src/main/scala directory for packages
  * use `Metals: Import build` to get VSCode to see mains for easier runs
* Day 1
  * `map` rules => `List(1,2,3,4).map(elem => elem+1)` or if you want to act directly on the element `(...).map(_.toString)` (_ is the element)
  * `Sorted` using `ORdering[Int]`
  * `List.slice` to make sublist
  * `foldLeft` to give `reduce`-like function an initial state/end type
    * `List(1,2,3,4).foldLeft(5)( (acc, cur) => acc+cur ) == 15`
  * `indexWhere` => find index that meets a predicate
  * `scala.io.Source.fromResource` to load resource files (under `src/main/resources`)
    * `getLines` to split by new line
    * `mkString` to turn into a full string
  * Adding an element to a list: `element +: list` (prepend -- preferred) `list :+ element` (append) or `list :: elem` / `elem :: list` (does the same thing)
  * `val/var`
    * `val` => immutable
    * `var` => mutable 
    * `var immutableStructure` -> you can set a new value 
    * `val mutableStructure` -> you can't reassign the value, but you can alter the struct
* Day 2
  * `Iterators` are like python's generators: they can run out. You either need to use `toList` or (from day 9) `duplicate` to keep them around
  * Pattern Matching (updated over several days):
    ```scala
    val value = 10 
    val checkValue = 8
    val rtn = value match {
        case 9 => "Can match against constant values"
        case `checkValue` => "Can match against variables, but requires special syntax"
        case x if x < 10 => "You can add guard statements by adding `if` after value"
        case _: String => "You can match on type as well"
        case 1 | 2 => "You can set up multiple scenarios that evaluate to the same result by using |"
        case _ => "Also has a nothing matched catch-all"
    }
    // rtn will have the result of the match
    // checks are performed in order, with the first that returns true being the case that's evaluated
    ```
* Day 3
  * can group elements in a list by using `grouped(x)`
    * `(1 to 10).grouped(3).toList => List( (1,2,3), (4,5,6), (7,8,9), (10)) `
* Day 4
  * You can do string interpolation by using `s"text $eval ${also eval}"`
  * `split` is a little tricky, because it return an Array, not a List
* Day 5
  * Add element to map: `("a" -> 1) + someMap`
  * Add maps together: `mapA ++ mapB`
  * `stripPrefix`/`stripSuffix` for trim but for start/end of line
  * `case class` is really for holding an immutable data structure
  * `normal constructor is in class definition: class X(val name: String, val age: Int)`
    * Could replace `val` with `var`
* Day 6
  * annotate with `@tailrec` on a method to optimize
* Day 7
  * Files in the same package don't need to be imported -- they're readily available
    * Metals/VSCode may have a fit if you rename one of these and create an unneeded import
  * `sealed class/trait` => all subclasses must be defiend in the same file. Best paired with `case classes` and using `trait` (instead of class). Good for something like an enum
* Day 8
  * `trait` is like an interface or abstract class -- they work pretty well
  * Note: This day has some simple performance testing to check a hand created array-backed set vs a hashset, so it's slightly trickier to read  
  * Ranges can be specified in a few ways
    * `(0 to n)` -> produces n+1 items (`n` is in the output set)
    * `(0 until n)` -> produces n items (`n` IS NOT in the ouptut set)
    * `(n to 0 by -1)` -> counts down n+1 times (n is in output set, and is unavoidably so when counting down)
  * If you want to loop through items, but exit early, you need to do this via recursion
* Day 9 (refined code, not a lot new)
  * Good example of super basic `sealed trait` (direction.scala)
  * Coplex drawing grid
* Day 10
  * Example of how to make an iterator
  * Expand a list into something suitable for varags with `:_*`
* Day 11
  * Decent functional approach -- I don't think there's any mutation here
  * A few examples of returning a function
  * A partial example of a alternate case constructor, but I didn't do it right because I ran into problems using it the way I wanted.

## Language Insights / Takes

* Scala has a lot of syntax
  * Some syntax is hard to remember
* Scala is pretty fast
* Hard to say really functional -- a lot of my code still looks somewhat procedural
* Vertical spacing is pretty important
* Due to the syntax, scala is not easy to intuit at a glance