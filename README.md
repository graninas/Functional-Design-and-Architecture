# Functional Design and Architecture
Code and materials for book "Functional Design and Architecture"

[Book page](https://graninas.com/functional-design-and-architecture-book)

### Support the book!

- [Patreon page](https://www.patreon.com/functional_design_and_architecture)
- [Paypal donations](https://www.paypal.me/graninas)

### What this book is about?

The book is focusing on these topics:
* Architecture modelling, requirements analysis, subsystems design from FP point of view;
* Embedded and external DSLs in domain modelling;
* Monads as subsystems with effects;
* Free monads as functional interfaces;
* Other types of functional interfaces;
* Inversion of Control in FP (using Free monadic eDSLs);
* Applicability of mainstream techniques and approaches such as UML, SOLID, GRASP;
* Interaction with impure subsystems.

The book is 70% done. 7 chapters are published online:

- [Book page](https://graninas.com/functional-design-and-architecture-book)
- [Book folder (Google Drive)](https://drive.google.com/open?id=0B1Rdr1fbS6M9SjlKUk1zMVNjOVU)
- [Acknowledges](https://docs.google.com/document/d/1bfSceZlCf5r4lRPg3Aif75ez_U1yuy-Kt6smFW_3Bhs/edit?usp=sharing)
- [Table of Contents](https://docs.google.com/document/d/1bh9Sa0rIGzU9Z88N_TJF6BtgHD_QLYdh1nK-yLKn_IU/edit?usp=sharing)
- [Chapter 1: What is software design?](https://docs.google.com/document/d/16pMEo0A-4GTnHqRn63yu73VqJ92M_pQYEd-t6tTiTcg/edit?usp=sharing)
- [Chapter 2: Architecture of the application](https://docs.google.com/document/d/1A0vnhwGxv1d4PyqdE0jPcutLcI_L5szlnPTqlhOSqMs/edit?usp=sharing)
- [Chapter 3: Subsystems and services](https://docs.google.com/document/d/1sRQ4766p2dtgj76IpZMz-rMwglLixm17Y-r2D7NcdHQ/edit?usp=sharing)
- [Chapter 4: Domain model design](https://docs.google.com/document/d/1UU-y4XaagexudLHWrrL9HeLClM6XobUqxRHL8Vdq2oc/edit?usp=sharing)
- [Chapter 5: Application state](https://docs.google.com/document/d/1v9RYc5GbUytS7shH0_8OWX_IOrliwCIH8-SMl8fGBSA/edit?usp=sharing)
- [Chapter 6: Multithreading and Concurrency](https://docs.google.com/document/d/1dUSt146dZ_t7SL1AHAm-VRsdYjC9gJb3totRR0qGy_0/edit?usp=sharing)
- [Chapter 7: Persistence](https://docs.google.com/document/d/1vhOMz6FMuEVUoURinbGIspi0_npIqV19LUOL0F7x0Mk/edit?usp=sharing)
- Chapter 8: Business logic design
- Chapter 9: Type level design
- Chapter 10: Testing

# Additional materials

### References list

[References List](https://drive.google.com/open?id=19nMC6zU0DBmX0JgiKecYziHO51TSOB1pgqvVbG0yf1Q)


### Andromeda

This is the first project created specially for the book. It demonstrates the ideas from the book in a standalone application simulating a SCADA system for spaceships.

[Andromeda SCADA-like system](https://github.com/graninas/Andromeda)

### Hydra

The [Hydra](https://github.com/graninas/Hydra) project is the second project created for the book. It's a framework similar to the Node framework for building multithreaded and concurrent applications in Haskell using the approaches and patterns from the book. It's interesting that the project has three different engines: Final Tagless, Free Monad and Church Encoded Free Monad, - and you can see the differences between these approaches by checking the different implementations of the same application.

### Node project  

The [Node](https://github.com/graninas/Node) is the most advanced production code in Haskell based the ideas of this book.

The Node Framework makes building of network acting nodes simple. It allows to handle concurrent state (with STM), work with KV database, host TCP, UDP and JSON-RPC servers, write network clients, interactible CLIs and more. The code of the nodes will be testable, safe and well-maintainable.

This framework can be a reasonable replacement for Cloud Haskell. I've wrote a detailed post about framework usage and architecture:

[Building network actors with Node Framework](https://gist.github.com/graninas/9beb8df5d88dda5fa21c47ce9bcb0e16)

### Automatic White-Box Testing with Free Monads (article and showcase)

This is a showcase project demonstrating an approach to white-box testing. I've developed this approach for Juspay and then ported it to Haskell (with help of other folks). The approach can be used for regression testing.

In it, Free Monadic scenarios can be recorded into a JSON recording which can be replayed against a new version of the code. A recording is a full footprint of a scenario and thus replaing of it will immediately spot changes and bugs.

[Automatic White-Box Testing Showcase](https://github.com/graninas/automatic-whitebox-testing-showcase)

### Juspay PureScript Presto Framework

I also was working for Juspay (India, Bangalore). In there, we have created two big projects in PureScript with the same central ideas of Free monads and architecture. The Presto framework for building mobile apps was published. The most of code (including Free eDSL and runtime) is designed and written by me:

[PureScript Presto](https://github.com/graninas/purescript-presto)

### Juspay PureScript Presto.Backend Framework

This is the second Free Monad framework. It resembles the Presto Framework and basically borrows its architecture. This framework is made by my colleauges, not me; although I've designed a recording-replying mechanism for white-box testing.

[PureScript Presto.Backend](https://github.com/graninas/purescript-presto-backend)

### Juspay PureScript Workflows Engine

Another project I was working on with some other great India developers was about distributed persistable resumable and typed workflows, in PureScript, Node.JS and RabbitMQ. This project is not published yet, but it also based on the Free monad architecture. Persistency of workflows and ability to pause them at any time is achieved due to special interpreters utilizing the RecorderT transformer.
