---
title: Running Haskell on Raspberry Pi
author: Alen Ribic
date: August 6, 2012
tags: haskell, raspberry pi
description: Few months ago, I ordered the Raspberry Pi, an ARM GNU/Linux box that fits into a palm of my hand and costs a mere $25. It arrived the past Monday and I couldn't wait to get my hands on it to see what it can do.
---
<div class="figure">
![](/images/HaskellLogo_s.png)![](/images/Raspi-PGB001_reasonably_small2.png)
</div>

Few months ago, I ordered the [Raspberry Pi](http://www.raspberrypi.org/), an ARM GNU/Linux box [1] that fits into a palm of my hand and costs a mere $25 [2].

It arrived the past Monday and I couldn't wait to get my hands on it to see what it can do.

Bringing R-Pi to life
------------

I began by installing [Raspbian](http://www.raspbian.org/), a free operating system based on Debian distribution optimized for the Raspberry Pi hardware.
This was a painless process.

I downloaded the Raspbian [image](http://www.raspberrypi.org/downloads) from the R-Pi official site and then I wrote the image to the SD memory card using the `dd` Unix utility. And yes, there is no built-in drive of course, SD card [3] serves as the storage device that the OS runs off, hence the optimizations required to effectively run the OS.

Popping the SD card into R-Pi and booting it up for the first time brought up the configuration wizard that enables some of the initial configuration of the box such as the locale settings, keyboard layout and the resizing of the SD card.

So I got my first login prompt and I launched an X session.
Playing around in the graphical environment for a bit, I can say that the little fella does more than a satisfying job. I'm pretty sure that it makes a very decent media player too.

But lets give that a bit of a pause and get on with the real fun stuff.
Raspbian comes pre-installed with the Python interpreter (2.7.x as of this writing), IDLE and some pre-packaged Python games.

Python is great but since I have my FP juices flowing, I thought I'd give installing Haskell a try.

R-Pi, meet my friend Haskell
------------

My initial thought was, "This is going to be a heck of a challenge." But after some simple Googling, I found the [R-Pi wiki entry](http://www.haskell.org/haskellwiki/Raspberry_Pi) on the Haskell official site.

Considering that I have Debian Wheezy (version that current Raspbian release is based on), GHC 7.4.1-3 is supported out of the box.

So I fire up an ssh session from my MacBook Air to R-Pi and run:

    sudo apt-get install ghc haskell-platform

This successfully installed the GHC compiler and the Haskell platform package.
So while I'm at it, I thought why not install Emacs and Haskell-mode.

    sudo apt-get install emacs haskell-mode

No problems here either.

Now the real excitement kicked in. I thought, lets launch the GHCi on the R-Pi and let the hacking begin.

So I ran `ghci` command and instead of getting the Prelude prompt, I got a:

    -bash: ghci: command not found

After some searching and asking the ever so helpful Haskellers at #Haskell for assistance, I quickly learned that there is no current support for GHCi on ARM type architecture. Effectively, this also means that there is no support for Template Haskell (TH).

The good news is that in 7.4.2, there is, for the first time, support for GHCi on ARM. This is really exciting. Debian distro also seems to have the experimental package for 7.4.2 for ARM which I haven't tried as yet.

Hacking a Ping server
------------

I wrote a socket server in Haskell that listens for the pings and concurrently [4] responds with the pongs.

Here is the server code:

~~~~~{.haskell}
import System.Environment (getArgs, getProgName)
import Control.Concurrent (forkIO)
import Network (Socket, PortID(PortNumber), withSocketsDo, 
                listenOn, accept)
import System.IO (hPutStrLn, hGetLine, hFlush)
import Control.Monad (forever)

main = withSocketsDo $ do
  args <- getArgs
  prog <- getProgName
  case args of
    [port] -> do
      socket <- listenOn $ PortNumber (fromIntegral (read port :: Int))
      putStrLn $ "Listening for pings on " ++ port
      handleRequest socket
    _ ->
      putStrLn $ "usage: " ++ prog ++ " <port>"

handleRequest s = do
  (handle, _, _) <- accept s
  forkIO $
    forever $ do
      input <- hGetLine handle
      processPing (hPutStrLn handle) (init input)
      hFlush handle
  handleRequest s

processPing f msg = do
  case msg of
    "ping" -> f "pong!"
    _      -> f "Did you mean `ping'?"
~~~~~

**Side note:** for beginners that the above code will run on any architecture supported by GHC.

In my ssh session to R-Pi, I ran:

    cat > pingserver.hs

And I pasted the above code followed by running the compiler:

    ghc -O2 --make pingserver.hs

The ghc flag `-O2` means to apply every non-dangerous optimisation, even if it results in significantly longer compile times.

**Side note:** talking about compilation, the compilation process on R-Pi takes a significantly longer time. In some cases it even runs out of memory [5].

I launched the ping server on my R-Pi and the client on my MacBook via telnet:

Server

    ./pingserver 5000

Client

    telnet 192.168.0.6 5000
    type `ping` followed by the return key

R-Pi responds with the "pong!" as expected.

Conclusion
------------

Installing and configuring R-Pi has been a really straightforward process.
Surprisingly, installing and configuring the Haskell environment on R-Pi has equally been a delight.

It is truly fantastic to see the work that has gone into supporting the targeting of Haskell on different architectures. Now with 7.4.2, ARM gets GHCi and more.

So where to from here? 

I have been dabbling with the `distributed-process`, a [Cloud Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf) implementation, in the past few weeks and thought that it would be really interesting to see if I can create a R-Pi Cloud backed by Cloud Haskell. I haven't done much thinking as yet here, especially regarding the benefit-cost ratio. 

This comes with some challenges considering that I'll probably need a few more R-Pi's and that Cloud Haskell utilizes TH [6] for adding syntactic sugar to cleanly support closure generators.

For questions and feedback, you can drop me an email or follow me on [twitter](https://twitter.com/alenribic).

**Source code:** [GitHub link](https://gist.github.com/3277051).

**[Updated: Mon, 06 Aug 2012]** fixed grammatical errors.

* * *

[1] Can't really call it a box considering the lack thereof.

[2] I have the Model B that costs $35

[3] I bought a cheap 4GB SD card that does a fine job

[4] Concurrently as in utilizing the `forkIO` of the `Control.Concurrent` package

[5] No surprise considering that R-Pi has only 256MB of RAM

[6] Template Haskell is not currently supported on ARM