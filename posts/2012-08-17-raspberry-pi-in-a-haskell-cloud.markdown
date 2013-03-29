---
title: Raspberry Pi in a Haskell Cloud
author: Alen Ribic
date: August 17, 2012
tags: haskell, raspberry pi
description: In the previous post, we walked through installing and running Haskell on Raspberry Pi. Today, we take this exploration further by evaluating the possibility of running R-Pi in a Haskell Cloud and we attempt to answer some important questions.
---
![](/images/CH_RPI_cloud_small.png)

In the previous [post](/posts/2012-08-06-running-haskell-on-raspberry-pi.html), we walked through installing and running Haskell on Raspberry Pi. Today, we take this exploration further by evaluating the possibility of running R-Pi in a Haskell Cloud and we attempt to answer some important questions.

tl;dr **We have Cloud Haskell successfully compiled and running on Raspberry Pi,  a credit card sized $25 computer.** Read on as we go into some of the details and towards the end, I'll try to explain the significance.

Before we begin, it would be worth giving a brief introduction to Cloud Haskell [1]:

> Cloud Haskell is a domain-specific language for cloud computing, implemented as a shallow embedding in Haskell. It presents the programmer with a computational model strongly based on the message-passing model of Erlang, but with additional advantages that stem from Haskell’s purity, types, and monads.

Building Cloud Haskell for R-Pi
------------------------

I noted in my previous conclusion that I've been dabbling with the `distributed-process` [2], a CH implementation. Some challenges were faced, notably the lack of support for TH on ARM type architecture [3], that needed overcoming in order to successfully compile the necessary CH packages.

With the help of Edsko de Vries, a core committer of the `distributed-process` project [2], we have the latest version of CH implementation that builds on R-Pi.

The latest update is available from Hackage so we can install the required packages directly from the repository.
With an ssh session to R-Pi, we can now build and install `distributed-process` directly on the device by running:

    cabal install --flags="-th" distributed-process

**Side note:** the `-th` cabal flag means to build the package without TH support. This is important as we have installed the GHC 7.4.1 previously that has no support for TH on ARM [3].

Now that we have the core packages installed, we'll install the `distributed-process-simplelocalnet` that offers local network node [6] discovery and as a zero-configuration backend, it gets us up and running very quickly:

    cabal install distributed-process-simplelocalnet

**Side note:** the build processes above will take considerably longer due to the limited resources of R-Pi [4].

There have been quite a few questions related to cross-compiling the required packages. As of this writing, there isn't an easy way to cross-compile with GHC 7.x [5]. 

One option at the moment is to install [QEMU](http://wiki.qemu.org/Main_Page), a generic and open source machine emulator and virtualizer, and build the [Raspbian](http://www.raspbian.org/) OS on it. This way we have an R-Pi emulated environment in which we can build packages that would take indefinitely long to build on R-Pi. Actually, come to think of it, this is great for people who have ordered their R-Pi and are waiting to take delivery. No need to wait, you can hack some Haskell goodness on it today!

Hacking a Distributed Ping
------------------------

Now that we have everything installed, I've written a distributed ping program that initializes both `master` and `worker` nodes [6] to play the `distributed` game of ping/pong. **The `master` will receive the list of available `worker` nodes and it will `map` over each `worker` node and asynchronously send a ping message to each. Workers in return will respond with pong messages that the `master` will simply direct to `stdout`.**

For the sake of brevity, I have excluded some of the boilerplate code that I will explain in a separate post, and which can be avoided using the forthcoming TH support for ARM (full source code can be accessed [here](https://gist.github.com/3368959)).

~~~~~{.haskell}
newtype Ping = Ping ProcessId
  deriving (Typeable, Binary, Show)

newtype Pong = Pong ProcessId
  deriving (Typeable, Binary, Show)

worker :: Ping -> Process ()
worker (Ping master) = do
  wId <- getSelfPid
  say "Got a Ping!"
  send master (Pong wId)

workerClosure :: Ping -> Closure (Process ())
workerClosure p = closure decoder (encode p)
  where
    decoder = ...

initialProcess :: String -> [NodeId] -> Process ()
initialProcess "WORKER" peers = do
  say $ "Peers: " ++ show peers
  pid <- getSelfPid
  register "slaveController" pid
  receiveWait []
initialProcess "MASTER" workers = do
  say $ "Workers: " ++ show workers
  pid <- getSelfPid
  
  forM_ workers $ \w -> do
    say $ "Sending a Ping to " ++ (show w) ++ "..."
    spawn w (workerClosure (Ping pid))
  say $ "Waiting for reply from " ++ (show (length workers)) ++ " worker(s)"
  replicateM_ (length workers) $ do
    let resultMatch = match (\(Pong wId) -> return wId)
      in do wId <- receiveWait [resultMatch]
            say $ "Got back a Pong from "
              ++ (show $ processNodeId wId) ++ "!"

main = do
  prog <- getProgName
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend (initialProcess "MASTER")
    ["worker", host, port] -> do
      backend <- initializeBackend host port rtable
      node <- newLocalNode backend 
      peers <- findPeers backend 50000
      runProcess node (initialProcess "WORKER" peers)
    _ -> 
      putStrLn $ "usage: " ++ prog ++ " (master | worker) host port"
~~~~~

Save the [source code](https://gist.github.com/3368959) as distributed-ping.hs file on your local computer and on R-Pi as we will compile it on both. Run the compile on both as follows:

    ghc -threaded --make distributed-ping.hs

The ghc flag `-threaded` tells GHC compiled to use the threaded runtime.

Time to test this out. I launched 2 `worker` nodes [6] on my R-Pi and 2 more `workers` on my MacBook Air:

R-Pi worker nodes

    ./distributed-ping worker 192.168.0.6 8081
    =>
    ... pid://192.168.0.6:8081:0:2: Peers: [nid://192.168.0.6:8081:0]

    ./distributed-ping worker 192.168.0.6 8082
    =>
    ... pid://192.168.0.6:8082:0:2: Peers: [nid://192.168.0.6:8081:0, nid://192.168.0.6:8082:0]

MacBook worker nodes

    ./distributed-ping worker 192.168.0.2 8081
    =>
    ... pid://192.168.0.2:8081:0:2: Peers: [nid://192.168.0.6:8081:0, nid://192.168.0.6:8082:0, pid://192.168.0.2:8081:0]
    ./distributed-ping worker 192.168.0.2 8082
    =>
    ... pid://192.168.0.2:8082:0:2: Peers: [nid://192.168.0.6:8081:0, nid://192.168.0.6:8082:0, pid://192.168.0.2:8081:0, pid://192.168.0.2:8082:0]

Now we need to start the `master` node that will distribute some work to our workers. From my MacBook:

    ./distributed-ping master 192.168.0.2 8080
    =>
    ... pid://127.0.0.1:8080:0:2: Workers: [nid://127.0.0.1:8081:0,nid://127.0.0.1:8082:0,nid://127.0.0.1:8083:0,nid://127.0.0.1:8084:0]
    ... pid://192.168.0.2:8080:0:2: Sending a Ping to nid://192.168.0.6:8081:0...
    ... pid://192.168.0.2:8080:0:2: Sending a Ping to nid://192.168.0.6:8082:0...
    ... pid://192.168.0.6:8081:0:4: Got a Ping!
    ... pid://192.168.0.2:8080:0:2: Sending a Ping to nid://192.168.0.2:8081:0...
    ... pid://192.168.0.6:8082:0:4: Got a Ping!
    ... pid://192.168.0.2:8080:0:2: Sending a Ping to nid://192.168.0.2:8082:0...
    ... pid://192.168.0.2:8081:0:3: Got a Ping!
    ... pid://192.168.0.2:8080:0:2: Waiting for reply from 4 worker(s)
    ... pid://192.168.0.2:8080:0:2: Got back a Pong from nid://192.168.0.6:8081:0!
    ... pid://192.168.0.2:8080:0:2: Got back a Pong from nid://192.168.0.6:8082:0!
    ... pid://192.168.0.2:8080:0:2: Got back a Pong from nid://192.168.0.2:8081:0!
    ... pid://192.168.0.2:8082:0:3: Got a Ping!
    ... pid://192.168.0.2:8080:0:2: Got back a Pong from nid://192.168.0.2:8082:0!

Thats it! **We've asynchronously distributed our ping messages [7] to our `worker` nodes and each reply message was received by the `master` as expected.**

Rationale
------------------------

Great, so we have a distributed ping example going but what is the point of all this? What makes Cloud Haskell a promising distributed system programming environment? And why should we bother to even think of building a cloud around Raspberry Pi systems, considering that these little fellas have substantially limited resources?

> Because Cloud Haskell brings some key contributions that I believe, in one form or another, will play a major role in the next generation of cloud computing. And because Raspberry Pi, through its sheer low cost and capability, has the potential to gain the greatest reach of any embedded system. 

**Some key contributions that CH brings forth:**

Firstly, and most notably, **CH enables across network transmission of first-class functions that capture their environment.** These functions are of pure [8] nature and therefore immensely improve the testability and informal reasoning. 

CH is strongly based on the message passing semantics of Erlang [7]. This brings a number of benefits such as guaranteed order of messages delivered between process pairs.

Through the language semantics, CH ensures that the programmer cannot accidentally mix up the **pure** and effectful code. 

CH employs types and **typed** message channels, as oppose to untyped, to help guarantee properties of programs.

Lastly, with implementations like `distributed-process` [2], the **separation of the transport layer** [10] **from the process layer** form part of the primary goals. This is paramount to achieving network transparency as it **enables processes to communicate across different types of networks**.

**Some final words on R-Pi:**

With the current demand [9], it is only a matter of time before we see these embedded, low cost, systems everywhere. This is important as I believe it should raise a question that is directly linked to distributed computing and the question is "what does the next generation of a cloud computational environment look like?" 

If we consider, for instance, that R-Pi systems are slowly but surely playing a role in robotics and R-Pi systems can quite simply, as demonstrated here, form a part of a distributed computing environment, then does that mean we could harness the computational power of robotic systems for alternative purposes when they are not being utilized for their primary use?

Conclusion
------------------------

With some great work that has gone into this area, **installing and configuring** Cloud Haskell across different architectures such as the R-Pi system (ARM) has become **a delightfully simple experience**. 

R-Pi demand is growing and as an embedded system, it is being deployed in many new innovative ways.

Can Cloud Haskell, and Haskell in general for that matter, harness this wave of embedded system deployments? With continued improvements and the information put forward in this writing, we can surely conclude that it can.

**What's next?**

I'll be writing a general and more detailed introduction to Cloud Haskell.

With quite a few questions related to cross-compiling and R-Pi, I'll begin digging into this area a bit deeper and I'll communicate the findings thereafter.

Finally, with the GHC 7.4.2 and ARM support, I will attempt to build Cloud Haskell with TH support and do a followup to this post.

For questions and feedback, you can drop me an email or follow me on [twitter](https://twitter.com/alenribic).

**Thanks** to Edsko de Vries for proofreading the draft of this post.

**Source code:** [GitHub link](https://gist.github.com/3368959).

**[Updated: Fri, 17 Aug 2012]** For comments, see the [Hacker News](http://news.ycombinator.com/item?id=4397385) entry.

***

[1] Introduction from the "Towards Haskell in the Cloud" paper by Jeff Epstein, Andrew Black, and Simon Peyton Jones. See: [http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf). See also the [official wiki page](http://www.haskell.org/haskellwiki/Cloud_Haskell).

[2] `distributed-process` is a Cloud Haskell implementation that has high development activity and is being headed by some very smart folks. See: [https://github.com/haskell-distributed/distributed-process](https://github.com/haskell-distributed/distributed-process)

[3] GHC 7.4.2 brings GHCi support to ARM.  A followup on this can be expected in the near future.

[4] As of this writing, R-Pi has a 700 MHz ARM processor, 256MB of RAM and has no built-in hard disk or solid-state drive, but uses an SD card for primary storage.

[5] This is currently one of the things on my todo list to further explore. See: [http://hackage.haskell.org/trac/ghc/wiki/CrossCompilation](http://hackage.haskell.org/trac/ghc/wiki/CrossCompilation)

[6] Node in CH can be thought of as an independent address space; a virtual server as oppose to physical.

[7] The semantics for message passing in `distributed-process` is based on the "A Unified Semantics for Future Erlang" paper by Hans Svensson, Lars-Ake Fredlund and Clara Benac Earle

[8] [http://en.wikipedia.org/wiki/Pure_function](http://en.wikipedia.org/wiki/Pure_function)

[9] As of this writing, more than 350,000 R-Pi boards were pre-ordered by April this year.

[10] [http://hackage.haskell.org/package/network-transport](http://hackage.haskell.org/package/network-transport)