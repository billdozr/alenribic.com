---
title: And we are live
author: Alen Ribic
date: August 4, 2011
tags: news, clojure
description: So it is the 4th of August 2011 and I have finally gotten around to breathing some life into this domain I purchased in early 2008. As the saying goes, better late than never. 
---
So it is the 4th of August 2011 and I have finally gotten around to breathing some life into this domain I purchased in early 2008. As the saying goes, better late than never. 

It has been an exciting past two weeks. ClojureScript [1], the new addition to the Clojure [2] ecosystem, was revealed. 
In a nutshell, ClojureScript is a Clojure to JavaScript compiler written by the author of Clojure, Rich Hickey and the Clojure/core team.

The basic rationale that gave birth to ClojureScript is that JavaScript has extensive reach, however, it has a number of acknowledged shortcomings. This is where ClojureScript seeks to address these shortcomings by replacing JavaScript with Clojure, a powerful general-purpose programming language.

Having enjoyed building web systems, with an exception to fiddling with browser compatibility issues and JavaScript's bad parts [3], I was more than keen on exploring the concept of utilizing a powerful programming language for exclusively programming across all application layers. This is how this website came to life.

It was a weekend exercise and boy was it a fun one.

Firstly, I must confess that **I really enjoy programming in Clojure**.
I think it largely has to do with the aha moment I had about three years ago that is best described by quoting Eric Raymond:

> Lisp is worth learning for the profound enlightenment experience you will have 
> when you finally get it; that experience will make you a better programmer for 
> the rest of your days, even if you never actually use Lisp itself a lot.

Secondly, the develop-build-test-deploy cycles in Clojure are very short and are generally performed interactively via the REPL (Read-Eval-Print-Loop) [4]. This results in high level of focus, or state of **flow** that truly maximizes productivity.

In a nutshell, with some basic design in mind, I settled on building the site with Clojure as a general-purpose programming language, ClojureScript as a JavaScript compiler for all front-end interactive bits, Enlive [5] as a selector-based HTML templating library and Moustache [6] as a micro web framework/internal DSL. Additionally, I employed Markdown [7] for all site content (static and dynamic) and PostgreSQL [8] database for storing all data that needed durable persistence.

Could I have build this site in other languages and technologies? Absolutely. However this was a chance to try something quite novel and different in my opinion. And I am very happy with the outcome. 

**Being able to program exclusively in Clojure across the application layers, is an enlightening experience.** In the future posts, I will share this enlivenment with clear rational why I believe that it is a more superior solution to web development than what is currently available in the main stream. I also plan to open source the code for this site once I have decoupled some of the site specific logic.

* * *

[1] ClojureScript - [https://github.com/clojure/clojurescript](https://github.com/clojure/clojurescript)

[2] Clojure - [http://www.clojure.org/](http://www.clojure.org/) 

Clojure is a dynamic programming language that targets the Java Virtual Machine and now the JavaScript VM. Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy and a powerful macro system. Clojure is predominantly a functional programming language, and features a rich set of immutable, persistent data structures.

[3]  JavaScript's bad parts such as global variables. There is no linker so all variables are tossed in to a same, common, namespace.

[4] Even though Clojure supports interactive development, it is a compiled language. There is no interpreter, Clojure code is compiled directly to VM byte-code.

[5] Enlive - [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)

[6] Moustache - [https://github.com/cgrand/moustache](https://github.com/cgrand/moustache)

[7] Markdown - [http://daringfireball.net/projects/markdown/](http://daringfireball.net/projects/markdown/)

[8] PostgreSQL - [http://www.postgresql.org/](http://www.postgresql.org/)