# Functional Design and Architecture
Code and materials for book "Functional Design and Architecture"

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

You might also want to check the Andromeda project that is a showcase project for this book:

[Andromeda SCADA-like system](https://github.com/graninas/Andromeda)

The `Node` project created by me and my team for Enecuum blockchain (Enecuum HK Limited, Hong Kong) is the most advanced production code in Haskell built using the ideas of this book:

[Node Framework](https://github.com/graninas/Node)

I also was working for Juspay (India, Bangalore). In there, we have created two big projects in PureScript with the same central ideas of Free monads and architecture. The Presto framework for building mobile apps was published. The most of code (including Free eDSL and runtime) is designed and written by me:

[PureScript Presto](https://github.com/graninas/purescript-presto)

Another project I was working on with some other great India developers was about distributed persistable resumable workflows, in PureScript, Node.JS and RabbitMQ. This project is not published yet.
