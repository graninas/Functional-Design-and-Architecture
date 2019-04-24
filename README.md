# Functional Design and Architecture
Code and materials for book "Functional Design and Architecture"

Book page: [link](https://graninas.com/functional-design-and-architecture-book)

### Support the book!

- [Patreon page](https://www.patreon.com/functional_design_and_architecture)
- [Paypal donations](paypal.me/graninas)

### What this book about?

The book is focusing on these topics:
* Architecture modelling, requirements analysis, subsystems design from FP point of view;
* Embedded and external DSLs in domain modelling;
* Monads as subsystems with effects;
* Free monads as functional interfaces;
* Other types of functional interfaces;
* Inversion of Control in FP (using Free monadic eDSLs);
* Applicability of mainstream techniques and approaches such as UML, SOLID, GRASP;
* Interaction with impure subsystems.

The book is 50% done. 5 chapters are published online:

- [Book folder (Google Drive)](https://drive.google.com/open?id=0B1Rdr1fbS6M9SjlKUk1zMVNjOVU)
- [Table of Contents](https://docs.google.com/document/d/1bh9Sa0rIGzU9Z88N_TJF6BtgHD_QLYdh1nK-yLKn_IU/edit?usp=sharing)
- [Chapter 1: What is software design?](https://docs.google.com/document/d/16pMEo0A-4GTnHqRn63yu73VqJ92M_pQYEd-t6tTiTcg/edit?usp=sharing)
- [Chapter 2: Architecture of the application](https://docs.google.com/document/d/1A0vnhwGxv1d4PyqdE0jPcutLcI_L5szlnPTqlhOSqMs/edit?usp=sharing)
- [Chapter 3: Subsystems and services](https://docs.google.com/document/d/1sRQ4766p2dtgj76IpZMz-rMwglLixm17Y-r2D7NcdHQ/edit?usp=sharing)
- [Chapter 4: Domain model design](https://docs.google.com/document/d/1UU-y4XaagexudLHWrrL9HeLClM6XobUqxRHL8Vdq2oc/edit?usp=sharing)
- [Chapter 5: Application state](https://docs.google.com/document/d/1v9RYc5GbUytS7shH0_8OWX_IOrliwCIH8-SMl8fGBSA/edit?usp=sharing)

# Additional materials

### References list

[List](https://drive.google.com/open?id=19nMC6zU0DBmX0JgiKecYziHO51TSOB1pgqvVbG0yf1Q)

### Andromeda

You might also want to check the Andromeda project that is a showcase project for this book:

[Andromeda SCADA-like system](https://github.com/graninas/Andromeda)

### Enecuum Node project  

The [Node](https://github.com/Enecuum/Node) project created by me and my team for Enecuum blockchain (Enecuum HK Limited, Hong Kong) is the most advanced production code in Haskell based the ideas of this book.

The Enecuum Node Framework makes building of network acting nodes simple. It allows to handle concurrent state (with STM), work with KV database, host TCP, UDP and JSON-RPC servers, write network clients, interactible CLIs and more. The code of the nodes will be testable, safe and well-maintainable.

This framework can be a reasonable replacement for Cloud Haskell. I've wrote a detailed post about framework usage and architecture:

[Building network actors with Enecuum Node Framework](https://gist.github.com/graninas/9beb8df5d88dda5fa21c47ce9bcb0e16)

### Juspay PureScript Presto Framework

I also was working for Juspay (India, Bangalore). In there, we have created two big projects in PureScript with the same central ideas of Free monads and architecture. The Presto framework for building mobile apps was published. The most of code (including Free eDSL and runtime) is designed and written by me:

[PureScript Presto](https://github.com/graninas/purescript-presto)

Another project I was working on with some other great India developers was about distributed persistable resumable workflows, in PureScript, Node.JS and RabbitMQ. This project is not published yet, but it also based on Free monad architecture. Moreover, persistency of workflows and ability to pause them at any time is achieved due to special interpreters utilizing a RecorderT transformer.
