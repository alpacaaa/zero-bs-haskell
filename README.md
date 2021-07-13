
<p align="center">
  <img alt="Zero Bullshit Haskell" src="exercises/test/logo.png" width="480">
</p>

<p align="center">
  Let's learn Haskell. For real this time. :rocket:
</p>

<p align="center">
  <a href="https://www.youtube.com/channel/UCJiwOqQi88UZCe8w7lwV4gw"><img alt="Zero Bullshit Haskell Youtube channel" src="https://img.shields.io/badge/Youtube-channel-ff0000.svg?style=flat"></a>
  <a href="https://alpacaaa.github.io/zero-bullshit-haskell/docs/"><img alt="Zero Bullshit Haddock documentation" src="https://img.shields.io/badge/docs-zero--bullshit-5E5184.svg?style=flat"></a>
</p>


&nbsp;


# Table of Contents

- [Introduction](#toc-introduction)
  - [What's Zero Bullshit Haskell](#toc-what-is-it)
  - [Exercises](#toc-exercises)
  - [Scope](#toc-scope)

- [Install Haskell in two minutes](#toc-install)
  - [The need for `stack`](#toc-need-stack)
  - [A word on `cabal`](#toc-word-cabal)
  - [Doing things with `stack`](#toc-things-stack)

- [What are types and why you should care](#toc-types)
  - [Using and abusing the type system](#toc-abusing-types)
  - [Show me the types](#toc-show-types)
  - [Types are your friends](#toc-types-friends)

- [Local dev environment and first exercise](#toc-solve-exercise)
  - [Getting ready](#toc-exercise-ready)
  - [Your first lines of Haskell](#toc-first-lines)
  - [Completing Exercise #01](#toc-completing-ex01)

- [Standard library and importing modules](#toc-imports-std-lib) 
  - [The Prelude](#toc-prelude)
  - [Qualified imports will keep you sane](#toc-qualify-imports)

- [How to debug Haskell code](#toc-debug-haskell)
  - [Everything is an expression](#toc-everything-expression)
  - [But... side effects?!](#toc-debug-side-effects)

- [Encoding and decoding JSON](#toc-encoding-decoding-json)
  - [A brief interlude on instances](#toc-interlude-instances)
  - [An even shorter interlude on constraints](#toc-interlude-constraints)
  - [`FromJSON` and `ToJSON`](#toc-fromjson-tojson)

- [Dealing with mutable state](#toc-dealing-state)
  - [Modeling state the functional way](#toc-modeling-state)
  - [A slight detour in OTP land](#toc-detour-otp)
  - [Let the runtime handle state for you](#toc-runtime-handle)

- [Install libraries from Hackage/Stackage](#toc-hackage-stackage)
  - [Fun times](#toc-fun-times)
  - [Hackage](#toc-hackage)
  - [Stackage](#toc-stackage)
  - [Packages that are not on Stackage](#toc-packages-not-stackage)
&nbsp;


# Exercises index

#### Beginner
- [Exercise #01 - Static String](https://alpacaaa.github.io/zero-bullshit-haskell/Ex01)
- [Exercise #02 - Echo](https://alpacaaa.github.io/zero-bullshit-haskell/Ex02)
- [Exercise #03 - Case match](https://alpacaaa.github.io/zero-bullshit-haskell/Ex03)
- [Exercise #04 - String manipulation](https://alpacaaa.github.io/zero-bullshit-haskell/Ex04)
- [Exercise #05 - On/Off switch](https://alpacaaa.github.io/zero-bullshit-haskell/Ex05)
- [Exercise #06 - Counter](https://alpacaaa.github.io/zero-bullshit-haskell/Ex06)
- [Exercise #07 - ShoppingCart](https://alpacaaa.github.io/zero-bullshit-haskell/Ex07)
- [Exercise #08 - ShoppingCart V2](https://alpacaaa.github.io/zero-bullshit-haskell/Ex08)

_work in progress_

&nbsp;


# <a id="toc-introduction"></a>Introduction

Haskell is a beautiful language.

It's not an easy language to learn though. Especially if you have years of experience in object oriented/imperative programming, getting into Haskell can be a bit of a pain.

This is my attempt at rectifying that. I want more and more people to learn about functional programming and Haskell in particular.

### <a id="toc-what-is-it"></a>What's Zero Bullshit Haskell

The goal is to have a learning resource that is simple and digestable, with a great deal of hands on exercises. Most Haskell resources tend to be super dense with a lot of theory — I want to approach things going the other way. I want to draw as many parallels as I can with the Javascript world because that's what most people are familiar with these days and **introduce concepts and theory only when necessary**.

I want you to get the bare minimum amount of information to get going with Haskell and complete the first few exercises. As you get more comfortable, further reading material will introduce new topics that will help you solve the problem at hand while polishing your existing solutions.

All the nasty and scary jargon is pushed as further as possible and presented only when absolutely necessary. If you complete all the exercises, you will have gained enough intuition beyond certain patterns that it'll only be a matter of naming them properly. This will help you strenghten your fp knowledge and understanding of all the beautiful concepts behind Haskell — but none of that will be smashed in your face from the get go. I know how challenging and overwhelming it can be for a beginner.

### <a id="toc-exercises"></a>Exercises

Exercises revolve around creating a webserver. It can be a very very simple webserver that just outputs `hello`, all the way to a concrete REST api that can handle state and connect to external services. We'll get there.

You can fiddle with the exercises with very little effort, as they come with a test runner that runs in the browser and hits `http://localhost:7879` — where your Haskell server will be listening. That's right, no need to install any extra bullshit. All you need to do is go to the exercises website and start firing some AJAX requests. Read [Local dev environment and first exercise](#toc-solve-exercise).

Perhpaps most importantly, all exercises have a **reference node.js implementation**. We'll discuss how each exercise could be solved using `express`, what the pitfalls of that implementation might be and how functional programming can help us in writing more robust code. Have a peak at [Exercise #01](https://alpacaaa.github.io/zero-bullshit-haskell/Ex01) to get an idea of how it works.

We're going to be using a Haskell [library](https://alpacaaa.github.io/zero-bullshit-haskell/docs) that is very similar in spirit and simplicity to `express`. In fact, I wrote it specifically for this project and I made sure it's devoid of any bullshit whatsoever. It's not production ready of course — we will transition to a more robust and widely used library later on.

### <a id="toc-scope"></a>Scope

This is not a Haskell tutorial.

You will not learn how to define types, functions or any of the syntax. Don't worry though! I think it's _manageable_ to figure this stuff out on your own. For instance, going through [Learn Haskell in Y Minutes](https://learnxinyminutes.com/docs/haskell/) a couple of times should be enough to get you started (note there's some bullshit at the end: ignore all the scary words, just play around with some of the examples).

With that being said, I'd like to have some content that covers the basics as well, eventually. Let me know if you'd like to see that happen. 

Zero Bullshit Haskell is about **overcoming the fear of types and start writing real Haskell code** that is simple to understand and doesn't look scary.

&nbsp;


# <a id="toc-install"></a>Install Haskell in two minutes

This is the first step, and it's just a `curl` away, but first a bit of clarification.

You can't really _install_ Haskell as much as you can't _install_ Javascript.

What you want to do, is install a Haskell compiler. The Haskell compiler that everybody uses is **GHC**: The Glorious Haskell Compiler. See that? It's called glorious, so it must be good.

GHC is like the Node.js of Haskell. For reasons we don't really care about, using GHC directly is a bit nasty. What we want is a tool that manages a Haskell project for us and that tool is called `stack`.

Install `stack` now and keep reading.

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

### <a id="toc-need-stack"></a>The need for `stack`

You can think of `stack` as `npm` or `yarn` — it's effectively solving a lot of the same problems. If you've already installed some version of GHC on your system, don't worry. Everytime you initialize a stack project, it will download and configure an appropriate version of GHC for you to use, so that you're not _stuck_ with a global install. Yes, it's nice like that.

### <a id="toc-word-cabal"></a>A word on `cabal`

There was a time when `stack` didn't exist and people used a tool called `cabal`. Those people experienced the so called _cabal hell_ and eventually created `stack` out of frustration. Recently, `cabal` has gotten a make over and it has gained back some of its original userbase.

Given that we have no idea what we're doing, we'll stick with `stack` because there is really nothing wrong with it and works great for what we need to accomplish here.

Under the hood, stack still uses `cabal`. You might also find a `.cabal` file in your projects. Leave that file alone (it's auto generated) and forget about `cabal`.

### <a id="toc-things-stack"></a>Doing things with `stack`

There are two commands that you're going to use all the time. To be honest, they are basically all you need.

* `stack build --file-watch`  
  Watch `.hs` files for changes and rebuild the project
* `stack run`  
  Build (if necessary) and run the project

&nbsp;


# <a id="toc-types"></a>What are types and why you should care

If you've read a Haskell blog post, joined some functional programming chat or attended a conference talk, chances are you experienced how much **people like to talk about types**. That's fine, once you get familiar with this stuff, you grow a weird and obsessive interest in pushing the _type system_, which is the set of rules that GHC follows to check that your program is correct.

But if you're like me, you don't.

### <a id="toc-abusing-types"></a>Using and abusing the type system

There is a fine line between making sure something is absolutely correct but unreadable/impossible to understand, and something that is less _safe_ (that is, has less guarantees at compile time) but is an actual piece of code that makes sense.

I personally don't get much excited about crazy types, but I still get to write Haskell for a living.

To me, writing simple Haskell that leverages only the absolute minimum amount of core features that are available in the language, is a joy to write, maintain and coming back to. I don't care about _fancy types_ and you won't find any in this series. This is as practical as it gets and there are fantastic resources out there when and if you'll want to get into that stuff. (Couple of personal suggestions: [i-am-tom/haskell-exercises](<https://github.com/i-am-tom/haskell-exercises>) and [Thinking with Types](<https://leanpub.com/thinking-with-types>))

### <a id="toc-show-types"></a>Show me the types

Here are a few sample types. You should familiarize yourself with all the different ways you can define a type.

```haskell
data Color
  = Blue | Orange | Green | Yellow

favoriteColor :: Color
favoriteColor = Green
```

A `Color` can be one of `Blue`, `Orange`, `Green` or `Yellow`.

`favoriteColor` is of type `Color` and has value `Green`.

```haskell
data Person
  = Person
      { name :: String
      , age :: Int
      , hair :: Color
      }

bob :: Person
bob = Person
        { name = "bob"
        , age = 97
        , hair = Orange
        }
```

A `Person` is a record. `bob` is of type `Person` .

```haskell
data Product
  = Book String Int
  | Movie String

oneBook :: Product
oneBook = Book "Some book title" 293

oneMovie :: Product
oneMovie = Movie "Some movie title"
```

A `Product` can either be a `Book`, which is made of a title of type `String` and the number of pages of type `Int`. Or it can be a `Movie`, which only has a title of type `String`.

We should use types to help us, so a better way of modelling this could be:

```haskell
data BookTitle
  = BookTitle String

data Pages
  = Pages Int

data MovieTitle
  = MovieTitle String

data Product
  = Book BookTitle Pages
  | Movie MovieTitle
  
oneBook :: Product
oneBook = Book (BookTitle "Some book title") (Pages 293)

oneMovie :: Product
oneMovie = Movie (MovieTitle "Some movie title")
```

### <a id="toc-types-friends"></a>Types are your friends

You model your problem/domain through types.

The first thing you should do before writing any actual implementation of your program is **figuring out your types**. If you get your types right, the implementation will follow naturally and GHC will be able to help you along the way because it knows exactly which types it's expecting. Your job is then to just fill in the holes.

The more you can prove about the correctness of your program at compile time, the less bugs you'll run into in production. Types allow you to keep the smallest context possible in your head and let GHC figure out what's going on. That's an invaluable asset when you want to reason about your program.

Type safety is all about that. We're making our code _safer_ through types. You can write some pretty complex and weird code through fancy types but that's bullshit, especially at this stage, so we're not getting into any of that.

A well modeled but simple type is just as useful. **Get your types right and the implementation will follow**. This is one of the main reasons Haskell is so appealing.

&nbsp;


# <a id="toc-solve-exercise"></a>Local dev environment and first exercise

All right, let's get down to business!

Make sure you have `stack` installed and create a new project — name it however you want.

```bash
stack new types-arouse-me
```

Now go into that folder and build. It might take a few minutes the first time.

```bash
stack build
```

While it's building, crack open `stack.yaml` in your freshly created project and tell it where to find the `zero-bullshit` library. This is the library you're going to need to complete the exercises. It's similar in spirit to `express` and `sinatra` in that it's super basic. I wrote it specifically for the Zero Bullshit Haskell project and believe me when I say I kept all the bullshit at bay!

```yaml
# stack.yaml

extra-deps:
- github: alpacaaa/zero-bullshit-haskell
  commit: master
  subdirs:
    - library
```

(Yes, we're pulling from Github. This library might end up on Hackage some day).

Then open `package.yaml` to require the package as a dependency.

```yaml
# package.yaml (recall this is basically package.json)

# Turn on some useful extensions while we're here!
# These should really be on by default. They just make
# the language nicer, don't worry about them.
default-extensions:
- OverloadedStrings
- DeriveGeneric
- DeriveAnyClass

dependencies:
- base # feel free to omit the version constraint
- zero-bullshit
```

### <a id="toc-exercise-ready"></a> Getting ready

Let's now take a peek at the first exercise.

[Exercise #01 - Static String](https://alpacaaa.github.io/zero-bullshit-haskell/Ex01)

I built all exercises so that they have an integration test that you can run straight from your browser. Head over to the [Exercise #01](https://alpacaaa.github.io/zero-bullshit-haskell/Ex01) page to see the `mocha` test runner ready to go. When you run the tests, they will hit `localhost:7879` which is where your server is going to be listening.

Obviously we have nothing running locally yet, so you should see a message saying that your server isn't running.

That's absolutely fine, we'll fix it in a minute. First, we want to run our vanilla stack-generated Haskell project just to check what's going on.

```haskell
stack run
> someFunc
```

What's this `someFunc` business? It's just a poorly worded hello world message. But more importantly, we have compiled and ran a Haskell application!


### <a id="toc-first-lines"></a> Let's write some Haskell

There are currently two files in our project `app/Main.hs` and `src/Lib.hs`.

Most of the times, you'll be editing stuff in the `src` folder — that's where 99% of your code should be. `app` is just for executables, which are thin entrypoints to your application (we have a single executable right now).

If you open `src/Lib.hs` you'll see this:

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

The very last line is the one we care about and hopefully it makes sense —  we're just printing some text to the console.

The first thing we want to do is import the `Zero.Server` module from the `zero-bullshit` package.

```haskell
import qualified Zero.Server as Server
```

And then we want to have our server listen for requests, even though it isn't able to handle any at the moment. So we end up with something like this:

```haskell
module Lib where

import qualified Zero.Server as Server

someFunc :: IO ()
someFunc = Server.startServer []
```

Now, give `stack run` another go and visit the integration tests page again. If your code is compiling and running correctly, the warning message should have disappeared!

That's right, the test runner has detected that our server is up and it's ready to run the tests. Let's give it a go, it will fail miserably.


### <a id="toc-completing-ex01"></a> Completing the first exercise

The test is making a `GET` request to `/hello` and is expecting a response with body `hello`. That shouldn't be too hard to implement. The main ingredients are `simpleHandler` and `stringResponse`.

**I highly encourage you to [watch the video](https://www.youtube.com/watch?v=Agp0qP96780) where I go through this** to understand what's going on. I'm pasting the solution here for reference (you'll find solution for other exercises linked in each exercise page).

```haskell
helloHandler :: Server.Request -> Server.Response
helloHandler _
  = Server.stringResponse "hello"

-- Change `someFunc` to `run` because we're not savages.
-- You'll have to update this in app/Main.hs as well.
run :: IO ()
run
  = Server.startServer
      [ Server.simpleHandler Server.GET "/hello" helloHandler
      ]
```

Execute `stack run` again and give the test another go, it should work.

Congratulation on getting this far!
Don't worry if you weren't able to follow along this exercise or any of the following ones.
I plan to record videos where I go through each exercise **in detail**, uncovering new concepts
and clearing up what we're doing differently compared to the node.js implementation. With time, you'll realize
how many things are uncomfortable/dangerous in the Javascript world, although I must admit you probably think Haskell
is the funny one right now. Stick around, it will be worth it!

&nbsp;


# <a id="toc-imports-std-lib"></a> Standard library and importing modules

Imagine you've just invented a new programming language. Now you need a standard library — a set of functions, types and a bit of blackbox magic baked into the compiler for developers to use.

Standard libraries are very hard to get right the first time. Once you make a bad decision, it's extremely difficult to **fix or deprecate** it because _everyone_ is using that thing. Effectively, all programming languages I've learned over the years have a somewhat broken standard library. So it shouldn't come as a surprise that the Haskell's one isn't great either, but it's not that bad.

### <a id="toc-prelude"></a> The Prelude

The standard library in Haskell is called `Prelude`.

It comes with the [base](http://hackage.haskell.org/package/base) package (you might have seen it listed as a dependency in your project, that's what it is). Anything in the `Prelude` module is imported by default, meaning you don't need an explicit `import` statement. Note that there are other modules in `base` and those do need importing (for example `Data.Maybe`). It might seem there's a lot of stuff in `base` — it is indeed a chunky package. But the day to day stuff you'll need is only a subset.

While not perfect, the stock Prelude will do just fine for our purposes. The main thing you have to watch out for is partial functions. In case of errors, these functions throw exceptions when they really shouldn't be. When you go through the exercises, you'll find warnings against usage of these functions and alternative safer implementations.

### <a id="toc-qualify-imports"></a> Qualified imports will keep you sane

It's often a good idea to _qualify_ your imports.

```haskell
import qualified Zero.Server as Server
-- In Javascript, this would be the equivalent of:
-- import * as express from 'express'
```

This way, you're not polluting the scope with a ton of functions and types. Most importantly, you'll always be able to tell where a function or type is coming from.

Sometimes it's handy to import specific types, or widely used functions, in which case it makes sense to use an unqualified import. But be careful, one of the things I struggled the most with as a beginner (among other things obviously) was having too many unqualified imports (it's just super confusing).

```haskell
-- The `Text` type and the `fromMaybe` function are super common.
-- It makes sense to import them unqualified.

import Data.Text (Text)
import Data.Maybe (fromMaybe)

example :: Text
example = fromMaybe ...


-- Next, the qualified version.
-- `Text.Text` is really uncomfortable to type over and over again and it just adds noise.
-- This is one of the instances where I would stick with unqualified imports.

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe

example :: Text.Text
example = Maybe.fromMaybe ...

```

&nbsp;


# <a id="toc-debug-haskell"></a>How to debug Haskell code

TLDR;  the closest equivalent to `console.log` is `traceShowId`.

```haskell
import qualified Debug.Trace as Debug

{-
  In JS we would write:
  const sum = (a, b) => {
    console.log(a)
    console.log(b)
    return a + b
  }
-}

sum :: Int -> Int -> Int
sum a b
  = (Debug.traceShowId a) + (Debug.traceShowId b)

-- >>> sum 1 2
-- 1
-- 2
```

Not quite the same is it? That's because everything in Haskell is an expression. (Note that the `Debug.Trace` module is part of the `base` package, so there's nothing extra to install)



### <a id="toc-everything-expression"></a>Everything is an expression

In Haskell there are no statements. At no point you can just say "do this" — your program is built as **expressions composed together**. So it gets tricky because `console.log` is very much a statement that you can basically drop anywhere and it works. But we have no such thing in Haskell so how can we work around that?

The trick is to "wrap" the value you want to log with any of the `trace` functions. The idea is that the `trace` function will print something to the console and give back the original value unchanged. This is cool because our expression **evaluates to the same result** whether it's wrapped in a `trace` function or not. In other words, these two implementations of `sum` are equivalent:

```haskell
sum :: Int -> Int -> Int
sum a b
  = a + b

sumWithLog :: Int -> Int -> Int
sumWithLog a b
  = (Debug.traceShowId a) + (Debug.traceShowId b)
```

They produce exactly the same result. In fact, the type signature for `traceShowId` is

```haskell
traceShowId :: Show a => a -> a
-- Ignore the `Show a =>` bit, we haven't really seen that yet.
```
Which means that whatever we give it (an `Int` in the `sum` function above) will be given back to us unchanged. Effectively, whether the `trace` function is there or not makes no difference!

Let's look at another example.

```haskell
{-
  In JS

  const sumItems = (items) => {
    const reducer = (acc, item) => {
      console.log(item)
      return acc + item
    }

    const sum = items.reduce(reducer, 0)
    console.log("the sum is " + sum)
    return sum
  }

  sumItems([1,2,3,4])
-}

sumItems :: [Int] -> Int
sumItems items
  = Debug.trace ("the sum is " ++ show sum) sum

  -- trace :: String -> a -> a
  -- print the first argument (String) and return the second argument (a)
  where
    reducer item acc
      = acc + (Debug.traceShowId item)
    sum
      = foldl reducer 0 items
```

I appreciate this might look a bit unfamiliar, but if you apply the "wrapping" logic it will make sense. As many other things in Haskell, it will take you some time getting used to. The trick is to realize that everything is an expression in Haskell and we have to work with this constraint to fit an extremely imperative concept ("log this, log that") into a purely functional language.



### <a id="toc-debug-side-effects"></a>But... side effects?!

You might not have realized it but: printing to the console is a _side effect_! How come none of the `trace` functions (all right, excluding `traceIO`) have `IO` in their signature??

The `putStrLn` function, which is used to print to the console, does correctly express in the type that it's _impure_ and some side effects will occur.

```haskell
putStrLn :: String -> IO ()
```

However, `trace` functions don't and that's because they are not type safe **on purpose**.

Remember that these functions are meant to be used to debug code, and most of the code we write will hopefully be pure. Let's pretend for a moment that `trace` functions _did_ run in `IO`.

```haskell
fictionalTrace :: String -> a -> IO a

sum :: Int -> Int -> Int
sum a b
  = a + b

-- If we now wanted to log something in the `sum`
-- function, we would have to modify its signature!

sumWithLog :: Int -> Int -> IO Int
sumWithLog a b
  = fictionalTrace ("the sum is" ++ show sum) sum
  where
    sum = a + b
```

This would be nuts. Not only you would have to edit your function everytime you wanted to log something, but all the places _where that function is used_ will need to change as well! You would soon go insane and sure enough just write _all of your functions_ in `IO`, just to avoid the pain.

That's not the code we want to write! After all, having `IO` everywhere wouldn't be much different than writing Javascript, because we would have lost the ability to **distinguish pure functions from impure ones**.

The takeaway is: yes, `trace` functions are a bit weird because they have side effects (printing to the console) but they don't run in `IO`. However that's on purpose, to make it more ergonomic for us developers in need of some sweet logging when fixing a bug.

For proper application logging you wouldn't use `trace` functions, just as much as you wouldn't use `console.log`. But that's a topic for another time.



&nbsp;


# <a id="toc-encoding-decoding-json"></a> Encoding and decoding JSON

We haven't mentioned JSON so far, but we better start looking into it so we can deal with the Real World. Going from JSON (decoding) and to JSON (encoding) in Haskell is not as easy as in Javascript, where everything is untyped. In Javascript we can just `JSON.parse()` and hope for the best, but we get no guarantees as to what we're actually getting back. Indeed, we hardly get any guarantees _at all_ — that's why we're learning Haskell after all!

Before we go any further, we need to quickly introduce a couple of things: _instances_ and _constraints_.

### <a id="toc-interlude-instances"></a> A brief interlude on instances

You might have seen type definitions followed by `deriving` , for example:

```haskell
data Person
  = Person
      { name :: String
      , age :: Int
      }
  deriving (Eq)

bob :: Person
bob = Person "bob" 69

alice :: Person
alice = Person "alice" 99

-- We can use the equality operator only because
-- `Person` has an `Eq` instance
-- (which we derived through `deriving`)
samePerson
  = if bob == alice
      then "Weird, they're the same"
      else "Duh, of course they're different"
```

Here, we're defining a new data type `Person`. However, the definition alone gives us a pretty limited type, because we wouldn't even be able to compare (`==`) two different `Person`s by default! So if we want to _enhance_ our type with equality, we need to tell the compiler that we'd like to derive an _instance_ for `Eq`, so that comparing `bob` and `alice` becomes possible.

Note that **the term _instance_ in OOP means something different**. In Haskell, an instance is just a way to describe what a type _is_ or _can do_. In this case, the `Person` type supports equality.

All you need to understand is that we can derive instances for our types so that we can do more useful things with them.

### <a id="toc-interlude-constraints"></a> An even shorter interlude on constraints

Instances alone wouldn't be that interesting — they become very useful when paired with constraints. You can _constraint_ the usage of a function so that a type parameter _must satisfy certain conditions_. Constraints are declared on the left of the fat arrow (`=>`). For example:

```haskell
-- You can only call this function IF the type has an `Eq` instance and a `Show` instance.
-- (`Show` is for turning values into Strings.)
compareAndPrint :: (Eq a, Show a) => a -> a -> String
compareAndPrint p1 p2
  = if p1 == p2
      then "They are the same person! " <> (show p1)
      else "Not the same person. You gave me" <> (show p1) <> " and " <> (show p2)

-- If we then derive `Show` as well as `Eq`,
-- the `Person` type will satisfy all the requirements of `compareAndPrint`.
data Person
  = Person
      { name :: String
      , age :: Int
      }
  deriving (Eq, Show)

-- >>> compareAndPrint bob bob
-- "They are the same person! Person {name = \"bob\", age = 69}"
```

Again, we can't rely on the `Person` type being showable by default, we need to derive a `Show` instance. And here's the super interesting thing: `compareAndPrint` can only work with *types that can be compared and shown*.

`(Eq a, Show a)` are constraints. They are necessary if we want to use `==` and `show` in our functions. An helpful way of looking at constraints is to read the full type signature in English:

```haskell
compareAndPrint :: (Eq a, Show a) => a -> a -> String
```

> If you want to use `compareAndPrint` , you have to give me two values of type `a`. But `a` can't be _any_ type! I'm looking for a type that has _at least_ (it can have more) instances for `Eq` and `Show`.

If that's clear, you have all you need to understand how JSON encoding/decoding works!

Side note, types in Prelude have been defined with common instances. So, for example, you could call `compareAndPrint` with `Int` and `String` because both satisfy the `Eq` and `Show` constraints.

```haskell
compareAndPrint 1 1
compareAndPrint "yes" "no"
compareAndPrint True True
```



### <a id="toc-fromjson-tojson"></a> `FromJSON` and `ToJSON`

How do we turn the `Person` type into JSON and how do we read a JSON value back into a `Person`? Easy, we just derive some more instances!

First of all, we need to install the `aeson` package (it's the Haskell package to deal with JSON basically). As you know, installing a package means opening `package.yaml`  and adding a new dependency.

```yaml
# package.yaml

dependencies:
- aeson # get this bad boy in!
- base
- zero-bullshit
```

Now we can derive `FromJSON` and `ToJSON` on our `Person` type!

```haskell
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Zero.Server as Server

data Person
  = Person
      { name :: String
      , age :: Int
      }
  deriving (Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

-- Notice the `Generic` instance.
-- It's required when you want to derive `FromJSON` and/or `ToJSON`.
```

Dope. How do we use them in practice? There are two functions available in the `Zero.Server` module that you'll end up using quite a lot: `decodeJson` and `jsonResponse`.

&nbsp;

```haskell
jsonResponse :: ToJSON a => a -> Response
```

> If you give me a type `a` that can be turned into JSON, then I'll produce a `Response` with the JSON representation of that type.

&nbsp;

```haskell
decodeJson :: FromJSON a => String -> Either String a
```

> If you give me a type `a` that can be parsed from a `String` (ie. the JSON encoded value), then I'll either give you an error (in case the JSON is invalid) or a value of type `a`.



In practice, if our server gets a request with some JSON encoded body, we can try to parse it into the type we expect.

```haskell
myHandler :: Server.Request -> Server.Response
myHandler req
  = Server.stringResponse result
  where
    body
      = Server.requestBody req
    result
      = case Server.decodeJson body of
          Left err -> "Failed to decode request body as a Person. It must be something else"
          Right p  -> "Yay! We have a person named: " <> (name p)
```

> **Side note**: where is that `name` function coming from? Recall that when you define a record in Haskell (`Person` in this case), one accessor function per field name gets automatically created as well. `name` and `age` are functions in scope here and we can use them to get values out of a record of type `Person`.

And, if we have a `Person` that we want to send as JSON, we can just do that (`jsonResponse` will also set the proper `Content-type` header).

```haskell
bob :: Person
bob = Person "bob" 69

myHandler :: Request -> Response
myHandler req
  = jsonResponse bob
```



The first few Zero Bullshit Haskell exercises don't deal with JSON at all, but given that exercises are about writing webservers, then you need to understand how to work with JSON. [Exercise #07](https://alpacaaa.github.io/zero-bullshit-haskell/Ex07) is all about putting what we just discussed into practice. If something isn't clear, join the discussion in the relevant issue!



&nbsp;

# <a id="toc-dealing-state"></a>Dealing with mutable state


Mutable state is one of the most common sources of bugs I regularly faced over the years. In Haskell everything is immutable (which is super nice) but sometimes we have to deal with mutable state nonetheless. How do we do that?

### <a id="toc-modeling-state"></a>Modeling state the functional way

In Javascript, we can just define a global variable and get away with it. For example, here's how we could implement a counter that gets increased with every request.

```javascript
var mutableCounter = 0

app.post('/increase-counter', (req, res) => {
  mutableCounter += 1
  res.send('Current count: ' + mutableCounter)
})
```

We could still define a _global_ `mutableCounter` value in Haskell, but there wouldn't be any way to update it.

```haskell
mutableCounter :: Int
mutableCounter = 0

counterHandler :: Server.Request -> Server.Response
counterHandler _ = ???
-- mutableCounter = mutableCounter + 1
-- No way the compiler would allow that!
```

Ok fine, GHC always has to ruin the party. Let's try to model a change of state as a function then. So far, our `counterHandler`  just takes a `Request` and returns a `Response`. A good first step would be adding the current counter value as an input to the function, so that we can use it!

```haskell
counterHandler :: Int -> Server.Request -> Server.Response
counterHandler currentCount _
  = Server.stringResponse $ "Current count: " ++ show (currentCount + 1)
```

This is fine, but `currentCount` is still an immutable value, so we can't do anything with it. Sure we can now return the current value to the client, but we can't update it.

What if we could let the server **manage state for us** instead?
Right now, our handler only returns a `Response`, but there's no reason why we couldn't return _extra information_. In fact, we can tell the server that along with the `Response`, we now also return an updated version of the state! The server will _somehow_ take that new value and do the actual mutation, we're not concerned with the details. The impure state update is hidden in library code and we can keep on writing nice pure functions. Turns out we don't really need mutable variables after all. :)

```haskell
counterHandler :: Int -> Server.Request -> (Int, Server.Response)
counterHandler currentCount _
  = (newCount, response)
  where
    newCount
      = currentCount + 1
    response
      = Server.stringResponse ("Current count: " ++ show newCount)
```

> Note how we now return a _tuple_ with two elements (the new state, the `Response`) instead of just the `Response`.

This is fantastic. All of our values are still immutable, we're just using **functions to model state updates**. The great thing about this is that `counterHandler` is still **super easy to test**. You feed it some state and check if the state you get out is the correct one. Have you ever written a test for a function that dealt with global mutable state (like in the JS example above)? I did, and then proceeded to learn Haskell.


### <a id="toc-detour-otp"></a>A slight detour in OTP land

[OTP](http://erlang.org/doc/system_architecture_intro/sys_arch_intro.html) is the set of indispensable libraries that you use to build Erlang/Elixir programs. Erlang is a functional language with immutable variables and, as you can imagine, wouldn't allow you to mutate any state.

One of the core abstraction in Erlang is the `gen_server`. A `gen_server` allows you to model a server by providing _callbacks_ that hook into the server lifecycle, so that you can implement your logic there.

Here's how the callback to handle requests looks like:

```erlang
 handle_call(Request, From, State)
```

It takes a `Request`, the process id of who's making the request and the **current state** of the server. What do you think the return value is?

```erlang
{reply, Reply, State1}
```

You guessed it, once again the trusty tuple!

Along with the Response (`Reply` in this case) we also return an updated version of the state, here referred to as `State1` to indicate it's going to be different than `State`.



### <a id="toc-runtime-handle"></a>Let the runtime handle state for you

We almost have all we need to build stateful request handlers. Let's drop some type signatures to understand what we're missing. In the `zero-bullshit` package, we create stateful handlers with the function `statefulHandler`.

```haskell
simpleHandler
  :: Method
  -> String
  -> (Request -> Response)
  -> Handler

statefulHandler
  :: Method
  -> String
  -> (state -> Request -> (state, Response))
  -> StatefulHandler state
```

This signature is similar to the familiar `simpleHandler` that we've used up until now. We know what `Method` and `String` are, so let's ignore those. The callback is interesting though (the function between parens). Note also how the return types are different. We wouldn't be able to pass a `StatefulHandler` directly to `startServer`, which only accepts `Handler`.

Let's bring back `counterHandler` and compare it to the callback we have here.

```haskell
                 (state -> Request -> (state, Response))
counterHandler :: Int   -> Request -> (Int,   Response)
```

Wait, it's exactly the same thing! `state` is what we call a _type variable_. We have seen a few of those already, when decoding JSON for example. We could have used `a` instead of `state` and it would have worked just the same, but it's good to be explicit with type variables when you can.

So `state` is a type, any type for that matter, it's not _constrained_ in any way so you can use whatever you want (we're using an `Int` here).

The last bit is `StatefulHandler state`. You might be wondering why `state` is there. Why can't our type just be `StatefulHandler`? The reason is we want to track the type of `state` an handler is compatible with. This is to **make sure we don't mix handlers** that work with `Int`s with handlers that work with `String`s for example. If it's still confusing, keep on reading and remember that `state` is just a type variable (meaning it can be replaced by `Int`, `String`, `Person` whichever type you want).

---

How do we get an `Handler` from a `StatefulHandler state` so that we can pass it to `startServer`? We use `handlersWithState`.

```haskell
handlersWithState :: state -> [StatefulHandler state] -> Handler
```

The server needs to know what the initial state is going to be, so that's the first argument we need to pass to `handlersWithState`. And then we can just pass a list of `StatefulHandler` provided that **they all work with the same `state` type**. That's why we need the type variable!

> `handlersWithState` is a _polymorphic_ function. It works with any type of `state`. Do you want `Int`? That's fine. Want `String`? Fine as well. But it **must match** the type of the `StatefulHandler`s you provide.
>
> We can use type variables in type constructors to carry _extra information_ at the type level. In this case, `StatefulHandler state` carries more information than just `StatefulHandler` because we can talk about the `state` as well. The better you model your types, the more guarantees you get at compile time. You're _reducing_ the number of invalid programs that the compiler will accept and get free errors (at compile time) when your types don't line up.
>
> That's where a type system is incredibly useful. If our program doesn't make sense at the type level, we're not going to be able to compile it and have bugs sneak into production code.

Putting it all together, we can use our `counterHandler` like this:

```haskell
initialState :: Int
initialState = 0

counterHandler :: Int -> Server.Request -> (Int, Server.Response)
counterHandler currentCount
  = (newCount, response)
  where
    newCount
      = currentCount + 1
    response
      = Server.simpleHandler ("Current count: " ++ show newCount)

main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState [counterHandler]
      ]
```

This is how we **model state in a functional way**.

At the end of the day, there's still going to be some mutable variable that gets updated under the hood, but the benefit of using this approach is we only get to deal with the **nice api on top**. When writing a `gen_server` in Erlang you don't have to think about how to manage state, everything is taken care of, just supply a callback. The same goes for the `zero-bullshit` server, you just have to pick a `state` of your liking, handle the `Request` and return an updated version of the `state` together with a `Response`.

---

# <a id="toc-hackage-stackage"></a>Install libraries from Hackage/Stackage

Haskell can be confusing. Or rather, the Haskell ecosystem can get pretty confusing. Turns out installing a library is super easy nonetheless, but it's worth going through it real quick.

### <a id="toc-hackage"></a>Hackage

In the beginning, it was Hackage. Hackage is just a website where users upload packages, pretty much as you would do with npmjs.com. You then list your dependencies in `package.yml` (which, you guessed it, is basically `package.json` in node.js) and you're good to go.

### <a id="toc-fun-times"></a>Fun times

Have you ever looked inside the `node_modules` folder?

In node.js, dependencies that have other dependencies are installed _within_ the dependency. You end up with `node_modules` nested into other `node_modules`. That's because a dependency can exist at **different** versions even in the **same** project.

Whether that's good or not I'll leave it to you to decide (hint, it's a bit shit). The important bit is that this is not how GHC works. GHC expects a specific package to exist only **once** at a **fixed** version (within the same project). This means that if two packages share a dependency, they'll have to agree on which version to use.

Now imagine 10 or 15 packages all depending on the same package, but with different version constraints. Dependency resolution becomes impossible. Welcome to *cabal hell*.

### <a id="toc-stackage"></a>Stackage

Remember how we talked about the nice people behind the build tool `stack` — well, who could have possibly invented *stackage*?

That's right, it's them striking again. (shout out FP Complete).

Their solution to the dependency problem (which, by the way, is in no way specific to Haskell, other languages have exactly the same problem) was to _curate_ a set of packages that are known to work together and put them into a package set.

In other words, the job of working out which packages build together (and most importantly, at which version) is taken care of.

This is massively useful! It means when you need to install a package, you just open `package.yml` and add a new entry under the `dependencies` field **without** pinning a specific version (unless you have a very good reason to).

### <a id="toc-packages-not-stackage"></a>Packages that are not on Stackage

First of all, is Hackage deprecated? Absolutely not, Hackage is still the source for all the packages in Stackage. And I'm just used to the way documentation looks on the Hackage website that I spend pretty much all of my time there and zero time on the Stackage one.

But it may happen that a package on Hackage is not present on Stackage. In fact, a lot of packages (either forgotten, never updated or not deemed production ready) can be found on Hackage but never make it to Stackage. It could also happen that a package was at some point in a Stackage package set, but because it failed to build or couldn't be maintained anymore, it got dropped in a more recent package set.

In a nutshell, you should be able to find everything you need, especially in the beginning. When you'll find yourself needing a package that's only available on Hackage, `stack` will warn you and will provide instructions on how to tweak your `stack.yml` file accordingly. It's super painless and working with a package set is really really nice.
