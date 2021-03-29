# Functional Design and Architecture
Code and materials for my book "Functional Design and Architecture"

- [Book page](https://graninas.com/functional-design-and-architecture-book)
- [Buy "Functional Design and Architecture" on Leanpub](https://leanpub.com/functional-design-and-architecture) (_e-book only; paper book delivery across the world is not possible any longer, see [this explanation post](https://graninas.com/2021/02/12/paper-book-shipment-problems/))

_N.B. I'm currently working on the second book, [Pragmatic Type Level Design](https://www.patreon.com/pragmatic_type_level_design). Your support will be highly appreciated!_

### What this book is about?

The book is focusing on these topics:
* Architecture modeling, requirements analysis, subsystems design from FP point of view;
* Embedded and external DSLs in domain modeling;
* Monads as subsystems with effects;
* Free monads as functional interfaces;
* Other types of functional interfaces;
* Inversion of Control in FP (using Free monadic eDSLs);
* Applicability of mainstream techniques and approaches such as UML, SOLID, GRASP;
* Interaction with impure subsystems.

# Additional materials

### References list

[References List](https://drive.google.com/open?id=19nMC6zU0DBmX0JgiKecYziHO51TSOB1pgqvVbG0yf1Q)

### Andromeda

This is the first project created specially for the book. It demonstrates the ideas from the book in a standalone application simulating a SCADA system for spaceships.

[Andromeda SCADA-like system](https://github.com/graninas/Andromeda)

### Hydra

The [Hydra](https://github.com/graninas/Hydra) project is the second project created for the book. It's a framework similar to the Node framework for building multithreaded and concurrent applications in Haskell using the approaches and patterns from the book. It's interesting that the project has three different engines: Final Tagless, Free Monad and Church Encoded Free Monad, - and you can see the differences between these approaches by checking the different implementations of the same application.

# Real-world applications of the ideas from the book

### Juspay's Haskell EulerHS Framework

This is a modern, full-fledged Free monad based Haskell framework for building web backends and console applications built by me and my team. EulerHS is an "older brother" of the Hydra framework and also is based on my ideas from the book. EulerHS is a default option for all the new projects at the financial company Juspay (Bangalore, India).

[EulerHS](https://github.com/juspay/euler-hs)

### Juspay's PureScript frameworks

Before EulerHS, we have've created two other Free monadic frameworks:

* [Juspay's PureScript Presto](https://github.com/juspay/purescript-presto)
* [Juspay's PureScript Presto.Backend](https://github.com/juspay/purescript-presto-backend)

The Presto.Core framework is used for building mobile apps. It was the first Free monadic framework I designed for Juspay using my ideas (with participation of other people).

Presto.Backend is intended for building web backends. It derives the design of components directly from Presto.Core. Presto.Backend was by my teammates, not me; although I've designed a recording-replying mechanism for white-box testing and improved the framework in a various places.

### Juspay PureScript Workflows Engine

Another project I was working on with some other great India developers was about distributed persistable resumable and typed workflows, in PureScript, Node.JS and RabbitMQ. This project is not published yet, but it also based on the Free monad architecture. Persistency of workflows and ability to pause them at any time is achieved due to special interpreters utilizing the RecorderT transformer.

### Node project  

The [Node](https://github.com/graninas/Node) is the most advanced production code in Haskell based the ideas of this book.

The Node Framework makes building of distributed applications simple. It allows to handle concurrent state (with STM), work with KV database, host TCP, UDP and JSON-RPC servers, write network clients, interactible CLIs and more. The code of the nodes will be testable, safe and well-maintainable.

This framework can be a reasonable replacement for Cloud Haskell. I've wrote a detailed post about framework usage and architecture:

[Building network actors with Node Framework](https://gist.github.com/graninas/9beb8df5d88dda5fa21c47ce9bcb0e16)

### Automatic White-Box Testing with Free Monads (article and showcase)

This is a showcase project demonstrating an approach to white-box testing. I've developed this approach for Juspay. It has its place in the Presto.Backend and EulerHS frameworks. The approach can be used for regression testing and not only.

The idea is that Free monadic scenarios can be recorded into a JSON file (recording) which can be then replayed against a new version of the code. Recordings represent a full footprint of a scenario and thus replaying of it will immediately spot changes and bugs.

[Automatic White-Box Testing Showcase](https://github.com/graninas/automatic-whitebox-testing-showcase)
