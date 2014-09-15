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

    Daviss-MacBook-Pro:~ davisfoote$ ghci
    GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude> 
  
By default, the prompt depends on whatever packages you have loaded. `Prelude` is the package of everything that is imported automatically in GHC. If you're working with a bunch of stuff, the prompt can get kind of unwieldy, so you can change it for the duration of your interactive session using `:set prompt "λ> "` (you can replace that string with whatever you want, but I think the lambda's pretty cute so that's what I'm gonna use). If you want to make that change permanent, add the same line to your `ghci.conf` file as outlined here: https://www.haskell.org/ghc/docs/7.2.2/html/users_guide/ghci-dot-files.html.

The interpreter works pretty much as you'd expect with arithmetic and whatnot. The most immediate thing you'll need to know to play around is the syntax for calling functions. In Haskell, a function call looks like an operator followed by its operands delimited by spaces. Some examples:

    λ> max 1 2
    2
    λ> exp 1
    2.718281828459045
    
Function application takes the highest precedence. 

    λ> max 1 2 + min 1 2
    3
    
In order to have expressions within operands to a function, you must use parentheses.
    
    λ> sin pi/2
    6.123233995736766e-17
    λ> sin (pi/2)
    1.0

Now let's take a look at lists. Much like lists in Scheme, Haskell lists are linked lists that are constructed using the Cons operator, `:`, terminated by the empty list, `[]`. Luckily, they come with some syntactic sugar that makes them look like Python lists.

    λ> 1:2:3:4:5:[]
    [1,2,3,4,5]
    λ> 5:[4,3,2,1]
    [5,4,3,2,1]
    
Characters in Haskell are denoted with single quotes. Lists of characters get special treatment in Haskell. In fact, "String" is synonymous with "list of characters". What I mean by that is that the type `String` is actually just a list of characters with more syntactic sugar to be easier to work with. Strings use double quotes.

    λ> ['h','e','l','l','o']
    "hello"
    λ> 'c':"ello"
    "cello"
    
One important note about Haskell lists that make them very different from Python or Scheme is that they are **homogeneously typed**. That is, you can have a list of `Integer`s or of `Double`s or of `Char`s or even of lists of `Integer`s, but you cannot have a list with elements of different types. If you try this, GHCi will blow up in your face. 

    λ> ['h','a','X',0,'r','z']

    <interactive>:20:14:
        No instance for (Num Char) arising from the literal `0'
        Possible fix: add an instance declaration for (Num Char)
        In the expression: 0
        In the expression: ['h', 'a', 'X', 0, ....]
        In an equation for `it': it = ['h', 'a', 'X', ....]
        
This is a type error. You're going to see **a lot** of these. They're the reason it will take ten tries to get your code to compile, and they're the reason that once it does compile, it will work the first time. More on that later. 

Haskell also has some neat syntax that allows you to tersely construct ranges of any enumerable type (for now, `Integer`s and `Char`s).

    λ> [1..5]
    [1,2,3,4,5]
    λ> [2,4..10]
    [2,4,6,8,10]
    λ> [5,4..1]
    [5,4,3,2,1]
    λ> ['a'..'e']
    "abcde"

There are a couple more cool things you can do with ranges, but I'll hold off on that until we've talked about laziness. Now that we've got our basic syntax out of the way, let's talk about functions.

User-Defined Functions
======================
Open up your favorite text editor and create a file called `sandbox.hs` (the name's not important, call it `sandwich.hs` if you'd rather. As long as it starts with `sand`). In Haskell, like most languages, `=` is used to assign values to variables. Unlike in most languages, though, `=` is not so much of an assignment as it is a definition. The distinction is that all values in Haskell are **completely immutable**. If you say that `x` is five then `x` is five, dammit, no matter what else happens. If you try to change it or provide another definition of `x`, everything will blow up again. 

To define a function, we list the name of the function followed by the names of its arguments, space delimited, on the left of the `=`, and we write an expression on the right. Whatever that expression evaluates to is the return value of the function. Let's go through some contrived examples. Type these definitions into `sandbox.hs`:

    square x = x * x
    sumSquares x y = square x + square y
    squareSum x y = square (x + y)

To play with them, switch back over to your GHCi session and type in this command:

    λ> :l sandbox
    [1 of 1] Compiling Main             ( sandbox.hs, interpreted )
    Ok, modules loaded: Main.
    λ> 

Let's try them out!

    λ> square 4
    16
    λ> sumSquares 4 5
    41
    λ> squareSum 4 5
    81

Well. Okay. Let's try to make that a little more exciting. Go back to `sandbox.hs` and define the function `bigness`.

    bigness x = if x > 50 then "big" else "small"
    
This is Haskell's equivalent of an `if` statement. If you're familiar with ternary expressions in any language, this is the same thing. If not, all that's different is that the entire thing is an expression, which means it must evaluate to something, which means the `else` always has to be there. It also means we can use it within other expressions like this:
    
    myMood sky = (if sky == "blue" then 'r' else 's') : "ad"
    
So let's play around with these a bit. Remember to `:l sandbox`.

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
            
Uh oh. Instead of `"smallbig"`, we got a type error. What's up with that?! Doesn't `+` concatenate strings? **NO!** This is right around the point where we need to start talking more seriously about types (if you're anxious, the operator you're looking for is `++`).

The Type System
===============
