---
title: An informal look at Java 8, Part 1
author: Alen Ribic
date: March 28, 2013
tags: java, lambda
description: With the new release of the Java programing language coming up, it'd be a good time to reflect on some of the major enhancements made to the language.
---

With the new release of the Java programing language coming up, it'd be a good time to reflect on some of the major enhancements made to the language.

Let's do a simple task of **computing a product of a sequence of even numbers**:

~~~~~{.java}
OptionalInt r = Streams.intRange(1, 11) // [1..10]
  .filter(n -> n % 2 == 0)
  .reduce((acc, n) -> acc * n);
out.println(r.isPresent() ? r.getAsInt() : 1);
~~~~~

First thing to bring to attention is streams [1].
Streams have been introduced to the standard library, which include the operations such as `map`, `reduce` and `filter`. Cool thing is that the stream library supports sequential and parallel operations.

In the snippet, we call the `intRange` static method to produce a sequential stream of integers from 1 to 10.
Then on that stream, we perform a filter operation to select the even numbers, followed by the reduce operation to accumulate the product value.

Lambda expressions [2] are one of the major and long awaited enhancements to the Java language.
In the snippet, the `filter` and the `reduce` operations are both termed higher-order functions (methods) as they both accept a lambda expression as arguments.
Since lambda expressions can be passed around as any other values, they come to the newly enhanced language as first-class citizens.

Lambda expressions in Java are lexically scoped with a constraint on the unbound variables (the local variables and parameters from the surrounding scope must be effectively final).
In terms of lambda/function subtyping, the return type is covariant [3] and the argument types are contravariant [3] as one would expect.

Finally, note the use of the Optional type, mostly analogous to the SML Option and Haskell Maybe types.
In the code snippet, the Optional type is a monomorphic type, however the polymorphic variant `Optional<T>` has been provided.

Even through a tiny snippet as the one above, it is quite clear that the **expressive power** gained through the addition of lambda expressions, stream operations and type inference will be more than welcomed.

For questions and feedback, you can drop me an email or follow me on [twitter](http://twitter.com/alenribic).

* * *

[1] The standard library, new package `java.util.stream`. There is no official Javadoc publicly hosted as yet.

[2] See [Project Lambda](http://openjdk.java.net/projects/lambda/)

[3] [Covariance and contravariance (computer science)](http://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science))