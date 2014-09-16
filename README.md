haskell-workshop-2014
=====================

Code and walkthrough for the Fall 2014 Hackers @ Berkeley Haskell workshop

Prerequisites
=============
This workshop assumes understanding of functional programming at the level of CS 61A. If you have not taken or are currently taking 61A, it may be hard to follow along. 

You should have already installed the Haskell Platform. If you have not, please do so by following the instructions at http://www.haskell.org/platform/

What You Will Learn
===================
I'm not going to make you a Rockstar Haskell Ninja Jedi in two hours. This workshop is, as I said, geared specifically toward those who have taken 61A but have never seen Haskell before. The implications of this are twofold:

1. Much of the focus of most introductory Haskell material is on things that you are already well familiar with, in particular higher order functions and recursion. We don't have to bother learning the ideas governing these concepts from the ground up, so we will be able to get into more unique aspects of the language and solving problems in a purely functional paradigm fairly quickly. 

2. As a consequence of that point, you're not going to have a lot of practice with the Haskell syntax by the time we do get into more interesting things. For that reason, the code we go through may be somewhat hard to follow the first time. If there's an overwhelming response of dissatisfaction with this, I'll slow down at the cost of not getting to everything planned. 

My goal then is not that you can sit down after the workshop and come up with all of this code again without any reference, but that, should you be intrigued enough to try to continue learning Haskell, you know where to look for further materials and you have a couple starting points for things to hack on that are both familiar and sufficiently interesting. You can look up the syntax as you need to (and if you're anything like me, you'll be doing that for at least a couple months). 

The Interpreter
===============
GHCi is the Haskell interpreter that comes packaged with the Platform. Open it up in your terminal by typing `ghci`. You should see something like this:

![GHCI](images/ghci.png?raw=true)
  
By default, the prompt depends on whatever packages you have loaded. `Prelude` is the package of everything that is imported automatically in GHC. If you're working with a bunch of stuff, the prompt can get kind of unwieldy, so you can change it for the duration of your interactive session using `:set prompt "λ> "` (you can replace that string with whatever you want, but I think the lambda's pretty cute so that's what I'm gonna use). If you want to make that change permanent, add the same line to your `ghci.conf` file as outlined here: https://www.haskell.org/ghc/docs/7.2.2/html/users_guide/ghci-dot-files.html.

The interpreter works pretty much as you'd expect with arithmetic and whatnot. The most immediate thing you'll need to know to play around is the syntax for calling functions. In Haskell, a function call looks like an operator followed by its operands delimited by spaces. Some examples:

```haskell
λ> max 1 2
2
λ> exp 1
2.718281828459045
```


Function application takes the highest precedence. 

```haskell
λ> max 1 2 + min 1 2
3
```
    
In order to have expressions within operands to a function, you must use parentheses.

```haskell
λ> sin pi/2
6.123233995736766e-17
λ> sin (pi/2)
1.0
```

Now let's take a look at lists. Much like lists in Scheme, Haskell lists are linked lists that are constructed using the Cons operator, `:`, terminated by the empty list, `[]`. Luckily, they come with some syntactic sugar that makes them look like Python lists. Also, strings (denoted by double quotes) are actually lists of characters (denoted by single quotes) with some additional syntactic sugar. Behold:

```haskell
λ> 1:2:3:4:5:[]
[1,2,3,4,5]
λ> 5:[4,3,2,1]
[5,4,3,2,1]
λ> ['h','e','l','l','o']
"hello"
λ> 'c':"ello"
"cello"
```
    
One important note about Haskell lists that make them very different from Python or Scheme is that they are **homogeneously typed**. That is, you can have a list of `Int`s or of `Double`s or of `Char`s or even of lists of `Int`s, but you cannot have a list with elements of different types. If you try this, GHCi will blow up in your face. 

```haskell
λ> ["this", "statement", "is", False]

<interactive>:2:29:
    Couldn't match expected type `[Char]' with actual type `Bool'
    In the expression: False
    In the expression: ["this", "statement", "is", False]
    In an equation for `it': it = ["this", "statement", "is", ....]
```

This is a type error. You're going to see **a lot** of these. They're the reason it will take ten tries to get your code to compile, and they're the reason that once it does compile, it will work the first time. More on that later. 

Now that we've got our basic syntax out of the way, let's talk about functions.

User-Defined Functions
======================
Open up your favorite text editor and create a file called `sandbox.hs` (the name's not important, call it `sandwich.hs` if you'd rather. As long as it starts with `sand`). In Haskell, like most languages, `=` is used to assign values to variables. Unlike in most languages, though, `=` is not so much of an assignment as it is a definition. The distinction is that all values in Haskell are **completely immutable**. If you say that `x` is five then `x` is five, dammit, no matter what else happens. If you try to change it or provide another definition of `x`, everything will blow up again. 

To define a function, we list the name of the function followed by the names of its arguments, space delimited, on the left of the `=`, and we write an expression on the right. Whatever that expression evaluates to is the return value of the function. Let's go through some contrived examples. Type these definitions into `sandbox.hs`:

```haskell
square x = x * x
sumSquares x y = square x + square y
squareSum x y = square (x + y)
```

To play with them, switch back over to your GHCi session and type in this command:

```haskell
λ> :l sandbox
[1 of 1] Compiling Main             ( sandbox.hs, interpreted )
Ok, modules loaded: Main.
λ> 
```

Let's try them out!

```haskell
λ> square 4
16
λ> sumSquares 4 5
41
λ> squareSum 4 5
81
```

Well. Okay. Let's try to make that a little more exciting. Go back to `sandbox.hs` and define the function `bigness`.

```haskell
bigness x = if x > 50 then "big" else "small"
```

This is Haskell's equivalent of an `if` statement. If you're familiar with ternary expressions in any language, this is the same thing. If not, all that's different is that the entire thing is an expression, which means it must evaluate to something, so the `else` always has to be there. It also means we can use it within other expressions like this:

```haskell    
myMood sky = (if sky == "blue" then 'r' else 's') : "ad"
```

So let's play around with these a bit. Remember to `:l sandbox`.

```haskell
λ> myMood "blue"
"rad"
λ> myMood "black"
"sad"
λ> bigness 123
"big"
λ> bigness (sumSquares 4 5) + bigness (squareSum 4 5)
<interactive>:5:26:
    No instance for (Num [Char]) arising from a use of `+'
    Possible fix: add an instance declaration for (Num [Char])
    In the expression:
        bigness (sumSquares 4 5) + bigness (squareSum 4 5)
    In an equation for `it':
        it = bigness (sumSquares 4 5) + bigness (squareSum 4 5)
```

Uh oh. Instead of `"smallbig"`, we got a type error. What's up with that?! Doesn't `+` concatenate strings? **NO!** This is right around the point where we need to start talking more seriously about types (if you're anxious, the operator you're looking for is `++`).

The Type System
===============
Haskell is **strongly typed**. This means, unlike Python, a value's type is rigid and can never change. Unlike Javascript, values of one type are not coerced into other types when a function is called on them. This may feel at first like a restriction, but it's actually extremely powerful. The type system is expressive enough that simply writing a type declaration for a function often serves as sufficient documentation. And like I said before, in almost all cases, if you make any mistake, the compiler will catch it and it will be a type error. That means if your code compiles, you probably didn't make any mistakes. 

To check the type of something in GHCi, you can do one of two things. Either `set +t`, which will make GHCi print out the type of every value you compute, or do `:t value`, which will just print the type of `value`. I'm gonna stick with the latter. 
    
```haskell    
λ> :t False
False :: Bool
λ> :t 'z'
'z' :: Char
λ> :t "derp"
"derp" :: [Char]
```

In the last line we see the syntax for a list of something, in this case `Char`s. 

Let's see what types of functions look like. 

```haskell
λ> :t myMood
myMood :: [Char] -> [Char]
```

What GHCi is telling us is that `myMood` is a function that takes a list of characters and returns a list of characters (i.e. a `String`). Pretty straightforward. To add a type declaration to your function, you can type the line that GHCi just gave you into `sandbox.hs` right above the function definition. Note that types are automatically inferred, so of course this is not necessary. Like I said though, type declarations serve as documentation, so it's a good habit to include them as much as possible. `String` and `[Char]` are synonyms, so you can instead type this:

```haskell
myMood :: String -> String
myMood sky = (if sky == "blue" then 'r' else 's') : "ad"
```

and GHC will know what you mean. Type synonyms serve as very terse but clear documentation, and we'll talk more about them later. 

Can you give a type signature for `bigness`? One important note: the type `Int` is for 32-bit signed integers. `Integer` represents unbounded integers. Either works here. As an exercise, try adding a type declaration to `bigness` and checking if it compiles by loading `sandbox` in your GHCi session.

There's something totally new that we have to talk about before we can look at functions of more than one argument. Functions in Haskell are curried by default. That means that a function of two arguments is actually a function that takes one argument and returns another function. Hence the type declarations for `sumSquares` should look like this (we'll assume we're working with small integers):

```haskell
sumSquares :: Int -> Int -> Int
```

Type that into `sandbox.hs` in the right place and add type declarations for the other functions in there. We'll see some examples pretty soon of how this can be really useful. 

We can treat operators as normal prefix functions by putting them in parentheses. This can be useful both to check the type of an operator in GHCi and to pass things like `*` and `+` into higher order functions.

```haskell
λ> :t (&&)
(&) :: Bool -> Bool -> Bool
λ> :t (:)
(:) :: a -> [a] -> [a]
```

What's going on here? The `:` operator is a function that takes in something of some type and a list of that same type and outputs a new list of the same type. `a`, then, is a **type variable**. When you actually use `:` in an expression, Haskell will infer from the context what the type `a` should be. Then `[a]` will be a list of that type. This means that `:` is a **polymorphic function**: it works on more than one type. We can actually add constraints to type variables that allow us to make assumptions about what they can do, which allows us to do some really cool stuff and build really useful domain-specific abstractions. Unfortunately, we don't really have time to get too far into that today. As an example to make what I'm saying more concrete before I abandon the subject, consider the type of `+`:

```haskell
λ> :t (+)
(+) :: Num a => a -> a -> a
```

Like `:`, `+` works on a type variable `a`. Unlike `:`, there is the constraint that `a` must be a numeric type. To learn more about this, read about typeclasses in one of the linked resources at the end. 

The question at hand, now, is how Haskell works with user defined types. 

User-Defined Types
==================
Let's consider a complex number example. In rectangular coordinates, a complex number is represented by a pair of the real part and the imaginary part. We haven't talked about tuples yet, but all that really needs to be said about them is that are very strictly typed. What I mean is that a tuple's type is uniquely defined by how many elements it has and what the types of those elements are. `(Int, Int)`, `(Int, Int, Int)`, and `(Int, Double)` are all distinct types. 

So our complex numbers can be represented as an ordered pair of type `(Double, Double)`! Brilliant. Now what would we ever want to do with a complex number? Maybe we want to find its magnitude. Okay, so let's define a function in `sandbox.hs`. Note that since we restricted the `square` function before to type `Int -> Int` before, it won't work here. 

```haskell
magnitude :: (Double, Double) -> Double
magnitude (re, im) = (re ** 2 + im ** 2) ** (0.5)
```

By the way, the syntax we're using here is a brilliant feature of Haskell, **pattern matching**. In the arguments to the function, we extract the real and imaginary parts by matching the input against the pattern `(re, im)` and making the corresponding bindings within the function. 

Okay, this works. But that type declaration for the function is not very descriptive, and we still haven't defined a new type. However, we can define a type synonym for `(Double, Double)` like this:

```haskell
type Complex = (Double, Double)
```

and then we can rewrite the type declaration for `magnitude` like this:

```haskell
magnitude :: Complex -> Double
```

Much better! Although, again, we still haven't defined a new type, only a type _synonym_. It's clear from the type declaration that the function is intended to be used on complex numbers, but since a complex number is represented only by an ordered pair of `Double`s, it will still work on anything else that we choose to represent this way. This could give us problems down the road if we had some unrelated piece of data that we represent as a pair of `Double`s, because then we'd have a whole class of functions that work on a type they're not intended to be used with. So let's actually define a new type. Delete the type synonym declaration above and add this instead:

```haskell
data Complex = Complex Double Double deriving (Show, Eq)
```

What we've done is define a type `Complex` as well as a data constructor `Complex`. These can be different, as we'll see momentarily. A data constructor is simply a function that takes in some arguments--here they are two `Double`s--and returns a value of the type on the left of the `=` that contains those values. Don't worry about the `deriving` part; here we're just telling Haskell that our `Complex` type can be both printed and equated naively. 

Now that we're no longer using a tuple, the pattern matching looks a little different. Change `magnitude` to look like this:

```haskell
magnitude (Complex re im) = (re ** 2 + im ** 2) ** (0.5)
```

A fairly straightforward change. Playing around with our new type may lend some intuition about how that's working. Load `sandbox.hs` into GHCi and try it out. By the way, you can define variables and functions in GHCi using the `let` keyword.

```haskell
λ> let x = Complex 2 3
λ> x
Complex 2.0 3.0
λ> magnitude x
3.605551275463989
```

As an exercise, try writing functions (along with their type declarations) that use pattern matching to extract the real and imaginary components of a complex number. 

As you can imagine, we'll often want to create data types that are much more complicated than that. Let's look at a binary tree example. Open `BinTree.hs` from the skeleton folder.

```haskell
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a) deriving (Show)
```

There are several things going on here. The first thing we see is a type variable `a`. That means BinTree is actually a **type constructor**. The type of a binary tree is uniquely determined by the type of its contents, which we've restricted to being homogeneously typed. Examples of concrete types we could make from this are `BinTree Int` and `BinTree Complex` (but make sure you have both files loaded before you try to make one of those). 


Laziness
========
infinite lists
map fold reduce, list comprehensions
how currying can be useful


Doing stuff
===========
IO


