# Functional Design and Architecture

It is a fundamental book about Software Design in statically typed functional languages, such as Haskell, F#, OCaml, PureScript, Elm, Scala. The main idea of this book is to provide a comprehensive source of knowledge, and a complete methodology for building real-world applications with functional ideas and patterns.

- [Info page about both editions](https://graninas.com/functional-design-and-architecture-book)
- [Functional Design and Architecture (First Edition, self-published)](https://leanpub.com/functional-design-and-architecture)
- [Functional Design and Architecture (Second Edition with Manning Publications)](https://www.manning.com/books/functional-design-and-architecture)

<p float="left">
  <a href="https://www.manning.com/books/functional-design-and-architecture">
    <img src="https://github.com/graninas/functional-declarative-design-methodology/blob/5a994d49d2de738105601f6dc3572df4b5a197d2/images/FDaA_cover.png" width="200"></a>
  <a href="https://leanpub.com/functional-design-and-architecture">
    <img src="https://github.com/graninas/functional-declarative-design-methodology/blob/9942840a5cb11c8844afa452a579031548d16225/images/FDaA_FE_cover.png" width="200"></a>
</p>

### Topics

* Functional architectures
* Functional design patterns and functional idioms
* Accidental and essential complexity of FP programs
* Design principles (SOLID, Low coupling / High cohesion; KISS etc.)
* Architecture modeling, requirements analysis, subsystems design from FP point of view
* Embedded and external DSLs in domain modeling (as a part of Domain-Driven Design)
* Monads as subsystems with effects
* Free monads as functional interfaces
* Other types of functional interfaces
* Inversion of Control in FP (using Free monadic eDSLs)
* Applicability of mainstream techniques and approaches such as UML
* Interaction with impure subsystems
* Command-line applications and backend applications
* Type-level programming
* Various testing approaches including property-based, functional, unit, integration etc.
* Approaches to build functional real-world concurrent application frameworks

# Additional materials

### Functional Declarative Design (FDD) Methodology

This article introduces Functional Declarative Design (FDD), a counterpart to Object-Oriented Design (OOD).

[Functional Declarative Design: A Comprehensive Methodology for Statically-Typed Functional Programming Languages](https://github.com/graninas/functional-declarative-design-methodology)

### Automatic White-Box Testing with Free Monads (article and showcase)

This is a showcase project demonstrating an approach to white-box testing. I've developed this approach for Juspay. It has its place in the Presto.Backend and EulerHS frameworks. The approach can be used for regression testing and not only.

The idea is that Free monadic scenarios can be recorded into a JSON file (recording) which can then be replayed against a new version of the code. Recordings represent a full footprint of a scenario and thus replaying it will immediately spot changes and bugs.

[Automatic White-Box Testing Showcase](https://github.com/graninas/automatic-whitebox-testing-showcase)

# Accompanying projects

### The Andromeda project

This is the first project created specially for the book. It demonstrates the ideas from the book in a standalone application simulating a SCADA system for spaceships.

[Andromeda SCADA-like system](https://github.com/graninas/Andromeda)

N.B. The project is quite outdated. Haskell has changed a lot since then. I'm working on its resurrection currently.

### The Hydra project

The [Hydra](https://github.com/graninas/Hydra) project is the second project created for the book. It's an application framework for building multithreaded and concurrent applications in Haskell using the approaches and patterns from the book. The project has three different engines: Final Tagless, Free Monad, and Church Encoded Free Monad, - and you can see the differences between these approaches by checking the different implementations of the same application.

# Real-world applications of the ideas from the book

### Juspay's Haskell EulerHS framework

This is a modern, full-fledged Free monad-based Haskell framework for building web backends and console applications built by my team. EulerHS is an "older brother" of the Hydra framework and also is based on my ideas from the book. EulerHS is a default option for all the new projects at the financial company Juspay (Bangalore, India).

[EulerHS](https://github.com/juspay/euler-hs)

### Juspay's PureScript frameworks

Before EulerHS, we have created two other Free monadic frameworks:

* [Juspay's PureScript Presto](https://github.com/juspay/purescript-presto)
* [Juspay's PureScript Presto.Backend](https://github.com/juspay/purescript-presto-backend)

The Presto.Core framework is used for building mobile apps. It was the first Free monadic framework I designed for Juspay using my ideas (with the participation of other people).

Presto.Backend is intended for building web backends. It derives the design of components directly from Presto.Core. Presto.Backend was by my teammates, not me; although I've designed a recording-replying mechanism for white-box testing and improved the framework in various places.

### Juspay's PureScript workflows engine

Another project I was working on with some other great Indian developers was about distributed persistable resumable and typed workflows, in PureScript, Node.JS, and RabbitMQ. This project is not published yet, but it is also based on the Free Monad architecture. Persistency of workflows and the ability to pause them at any time is achieved due to special interpreters utilizing the RecorderT transformer.

### Node project  

The [Node](https://github.com/graninas/Node) is the most advanced production code in Haskell based on the ideas of this book.

The Node Framework makes the building of distributed applications simple. It allows handling concurrent state (with STM), working with the KV database, host TCP, UDP, and JSON-RPC servers, writing network clients, interactive CLIs, and more. The code of the nodes will be testable, safe, and well-maintainable.

This framework can be a reasonable replacement for Cloud Haskell. I've written a detailed post about framework usage and architecture:

[Building network actors with Node Framework](https://gist.github.com/graninas/9beb8df5d88dda5fa21c47ce9bcb0e16)

### References list

[References List](https://drive.google.com/open?id=19nMC6zU0DBmX0JgiKecYziHO51TSOB1pgqvVbG0yf1Q)
