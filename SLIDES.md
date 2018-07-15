---
title: Haskell from the Inside Out
---

## Haskell from the Inside Out

* Michael Snoyman
* VP of Engineering, FP Complete
* FLIP, Tel Aviv, Israel
* July 23, 2018

<div style="text-align:center">
<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>
</div>

---

## Get the code!

* This is a workshop, right?
* Get the code and tools!
* `git clone https://github.com/snoyberg/haskell-inside-out`
* Follow instructions in README.md
* Kick it off now, may take some time to download

----

## Format

* This is _not_ a lecture or talk
* This is an interactive workshop
* I'm going to ask questions
* There will be exercises to play with
* תרגישו בבית

----

## Haskell is _weird_

* Computers are inherently imperative
* Most programming languages are imperative
* Haskell is stubbornly _not_ imperative
* Functional, pure, lazy, immutable...
* Goal today:
    * Understand where this weirdness comes from
    * See why this weirdness is really useful

----

## Purely functional

* Haskell is a purely functional programming language
* Most other weird things derive from that
* Purely functional is nice
    * Better testing
    * Referential transparency (reason about code)
    * Allows some optimizations
    * Trivially create Software Transactional Memory
* Not obvious how to make this work in a programming language

----

## Diving in

* Start with just one constraint: functions are pure
* We'll define that a bit better as we go
* Let's see how this affects our ability to write normal code

---

## Pure math

We want to evaluate this arithmetic expression

```
(2 + 3) * (4 + 5)
```

1. Do it in your heads, try to note how you procssed it
2. Let's write up an answer in imperative pseudocode

----

## Imperative solution

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

* Any objections?
* Not terribly different from how the processor itself would do things

----

## Variation 1

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
y = 4 + 5;
x = 2 + 3;
z = x * y;
return z;
```

----

## Variation 2

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
z = x * y;
x = 2 + 3;
y = 4 + 5;
return z;
```

----

## Variation 3

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
x1 = 2 + 3;
x2 = 2 + 3;
x3 = 2 + 3;
x4 = 2 + 3;
y = 4 + 5;
z = x4 * y;
return z;
```

----

## Variation 4

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
w = 1 + 2;
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

----

## Takeaways

* Some programs do different things internally, but externaly behave the same
* Must calculate `x` and `y` before `z`
* Can calculate other, irrelevant things like `w`
* Can recalculate `x` as many times as desired

Let's do something similar...

---

## Say hi

* Get a name from the user
* Print the name back out
* We'll use imperative pseudocode again

----

## Basic solution

```
print("What's your name?");
str = getString();
print(str);
```

----

## Variation 1

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
str = getString();
print("What's your name?");
print(str);
```

----

## Variation 2

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
print(str);
print("What's your name?");
str = getString();
```

----

## Variation 3

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
print("What's your name?");
str1 = getString();
str2 = getString();
str3 = getString();
str4 = getString();
print(str4);
```

----

## Variation 4

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
print("What's up?");
print("What's your name?");
str = getString();
print(str);
```

----

## Takeaways

* Must call `getString()` and set `str` before running `print(str)`
* Cannot run other, irrelevant things like `print("Whats' up?")`
* Cannot run `getString()` multiple times

Compare to our previous takeaways:

* Must calculate `x` and `y` before `z`
* Can calculate other, irrelevant things like `w`
* Can recalculate `x` as many times as desired

What gives?

---

## Discussion

* What's the difference between arithmetic and input/output
* What's the result of running `2 + 3`?
* What's the result of running `getString()`?
* What's the result of running `print("What's your name?")`?

----

## What's a function?

* Maps input to output
* What are the input and output to the following:
    * Plus function `+`
    * `getString`
    * `print`
    * `rollDie`

----

## Results of evaluating

* Only result from evaluating `2 + 3`: the number 5
* Two results from evaluating `getString()`
    * Some I/O (a prompt to the user)
    * A string value
* Who cares if we have the number 5 multiple times? We ignore the
  unneeded ones!
* But we can't ignore the extra I/O from `getString`

----

## Back to order of evaluation

* Does it matter if we evaluate `2 + 3 = 5` before `4 + 5 = 9`?
    * __No!__
* Does it matter if we print `What's your name` before prompting for a
  name?
    * __Yes!__
* Same rules seem to apply to repeated evaluation and reordered
  evaluation

----

## Focus on math

`(2 + 3) * (4 + 5)`

* Evaluate each subexpression as many times as desired
* Rearrange order of evaluation for subexpressions as desired
* _Must_ evaluate subexpressions before full expression
* Time for some terminology
    * Pure function
    * Data dependency

----

## Pure function

* A function in the mathematical sense
* Deterministic output for given input
* Cannot observe that the function has been run besides having a
  result
    * E.g., no I/O
    * Little bit of a lie: we know the CPU got hotter :)
* Different from what we call "functions" in most programming languages!

----

## Data dependency

* Impossible to figure out `x * 2` without knowing `x`
* We have a _data dependency_ on `x`
* Challenge: evaluate `(3 + 4) * 2`, but don't figure out `3 + 4`

---

## Can we program purely functionally?

* We already saw that this fell apart for `print` and `getString`
* Why? They aren't purely functional!
* Extra result with no data dependency to force order of evaluation
* But pure functions sound so cool!

----

## The state of the world

* `print` affects the state of the world (changes the console)
* `getString` is affected by the state of the world (user input)
* Can we somehow represent that concept in a "purely function" way?
* Can we create some data dependency out of this?

----

## Fake it

Imagine this crazy rewrite:

```
fn main(iostate1):
  iostate2 = print("What's your name?", iostate1)
  (iostate3, str) = getString(iostate2)
  iostate4 = print(str, iostate3)
  return iostate4
```

* Cannot try to put `getString` before first print, because...
* We created a data dependency!
* Various `iostate` values force an order of evaluation

---

## Exercise 1

* Go into the exercises directory
* Run `stack build`
* Run `stack runghc ex1-fake-it.hs`
* Make it run :)

```haskell
getString :: IOState -> (IOState, String)
print :: String -> IOState -> IOState
```

* `getString` takes one argument, returns two values
* `print` takes two arguments, returns one value

----

## How was that?

* It works, but it's ugly
* We're making our life really difficult
* Can we do better? Yes
* Let's make a _pattern_

----

## Type aliases

Haskell lets us create type aliases with variables

```haskell
getString :: IOState -> (IOState, String)
print     :: String -> IOState -> IOState
```

Include "dummy" value in `print`:

```haskell
getString :: IOState -> (IOState, String)
print     :: String -> IOState -> (IOState, ())
```

And now:

```haskell
type Action a = IOState -> (IOState, a)
getString :: Action String
print     :: Action ()
```

A bit easier to see what's happening

---

## Exercise 2

* Convert our previous example to the new `Action` type alias
* Use `stack runghc ex2-fake-it-action.hs`
