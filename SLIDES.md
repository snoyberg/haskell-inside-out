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

---

## Haskell is _weird_

* Computers are inherently imperative
* Most programming languages are imperative
* Haskell is stubbornly _not_ imperative
* Functional, pure, lazy, immutable...
* Goal today:
    * Understand where this weirdness comes from
    * See why this weirdness is really useful

---

## Purely functional

* Haskell is a purely functional programming language
* Most other weird things derive from that
* Purely functional is nice
    * Better testing
    * Referential transparency (reason about code)
    * Allows some optimizations
    * Trivially create Software Transactional Memory
* Not obvious how to make this work in a programming language

---

## Diving in

* Start with just one constraint: functions are pure
* We'll define that a bit better as we go
* Let's see how this affects our ability to write normal code

---

## Non-trick question

How should this program (pseudocode) behave?

```
print("What is your name?")
name = getString()
print("What is your age?")
age = getInt()
print(name + " is " + age + " years old")
```

Is it the same as this?

```
age = getInt()
name = getString()
print("What is your age?")
print("What is your name?")
print(name + " is " + age + " years old")
```

Why not?

---

## Let's do math

Consider the math expression `(2 + 3) * (4 + 5)`

```
x = 2 + 3
y = 4 + 5
return (x * y)
```

And is this the same?

```
y = 4 + 5
x = 2 + 3
return (x * y)
```

---

## Does order matter?

* First example: absolutely! We'll get the wrong user interaction otherwise
* Second example: not at all! Math is math!

Can we come up with some general rules about when we can rearrange
order of evaluating things?

---

## Repeated evaluation

Does this code work the same way?

```
print("What is your name?")
name = getString()
name = getString()
print("What is your age?")
age = getInt()
print(name + " is " + age + " years old")
```

How about this?

```
x1 = 2 + 3
x2 = 2 + 3
x3 = 2 + 3
x4 = 2 + 3
y = 4 + 5
return (x4 * y)
```

---

## Results of evaluating

* Only result from evaluating `2 + 3`: the number 5
* Two results from evaluating `getString()`
    * Some I/O (a prompt to the user)
    * A string value
* Who cares if we have the number 5 multiple times? We ignore the
  unneeded ones!
* But we can't ignore the extra I/O from `getString`

---

## Back to order of evaluation

* Does it matter if we evaluate `2 + 3 = 5` before `4 + 5 = 9`?
    * __No!__
* Does it matter if we print `What's your name` before prompting for a
  name?
    * __Yes!__
* Same rules seem to apply to repeated evaluation and reordered
  evaluation

---

## Push the envelope

Can we rewrite:

```
x = 2 + 3
y = 4 + 5
return (x * y)
```

To

```
z = x * y
y = 4 + 5
x = 2 + 3
return z
```

* Probably not
* We don't know `x` and `y` when we try to calculate `z`
* So order of evaluation _does_ matter

---

## Focus on math

`(2 + 3) * (4 + 5)`

* Evaluate each subexpression as many times as desired
* Rearrange order of evaluation for subexpressions as desired
* _Must_ evaluate subexpressions before full expression
* Time for some terminology
    * Pure function
    * Data dependency

---

## Pure function

* A function in the mathematical sense
* Deterministic output for given input
* Cannot observe that the function has been run besides having a
  result
    * E.g., no I/O
    * Little bit of a lie: we know the CPU got hotter :)

Does this explain our rules from before?

---

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

---

## Fake it

Imagine this crazy rewrite:

```
fn main(iostate1):
  iostate2 = print("What is your name?", iostate1)
  (name, iostate3) = getString(iostate2)
  iostate4 = print("What is your age?", iostate3)
  (age, iostate5) = getInt(iostate4)
  iostate6 = print( name + " is " + age + " years old"
                  , iostate5)
  return iostate6
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
getString :: IOState -> (String, IOState)
print :: String -> IOState -> IOState
```

* `getString` takes one argument, returns two values
* `print` takes two arguments, returns one value

---

## How was that?

* It works, but it's ugly
* We're making our life really difficult
* Can we do better? Yes
* But let's deal with age next
