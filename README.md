
# ¤ Fudgets ¤

[Fudgets](https://www.altocumulus.org/Fudgets/)
is primarily a Graphical User Interface Toolkit implemented in Haskell
on top of its own binding to the
[Xlib library](https://tronche.com/gui/x/xlib/) of the
[X Windows system](https://www.x.org/).
Fudgets also makes it easy to create client/server applications that
communicate via the Internet.
[The Hello world program](https://www.altocumulus.org/Fudgets/Intro/ex1.html)
fits on a single line:

```haskell
main = fudlogue (shellF "Hello" (labelF "Hello world!"))
```

The key abstraction is
[*the fudget*](https://www.altocumulus.org/Fudgets/Intro/concept.html).
A fudget is a *stream processor* with
high-level and low-level streams. The high level streams are used for
communication between fudgets within a program.
The low level streams are for communication with the I/O system.

> ![](https://www.altocumulus.org/~hallgren/WebFudgets/doc/P/the_fudget2.jpg)

Fudgets are combined using various combinators for parallel composition,
serial composition and loops.

> ![](https://www.altocumulus.org/~hallgren/WebFudgets/doc/P/fudget_plumbing2.jpg)

Fudgets was originally implemented in Lazy ML in the early 1990s,
then converted to Haskell. It was thus designed before monadic IO was
introduced in Haskell and early versions did not make use of Haskell's
type classes at all.

## Documentation

- [Your first 8 Fudgets program](https://www.altocumulus.org/Fudgets/Intro/).
  Gentle Introduction.
- [Fudgets User's Guide](https://www.altocumulus.org/Fudgets/userguide.html).
  Naming conventions and some other practical things.
- [Fudget Library Reference Manual](https://www.altocumulus.org/Fudgets/Manual/). (Haddock did not exist back when Fudgets were created.)
- [The FPCA-93 paper about Fudgets](https://www.altocumulus.org/Fudgets/fpca93-abstract.html),
  the first publication describing Fudgets.
- See the [Fudgets home page](https://www.altocumulus.org/Fudgets/) for more info.

## Installing Fudgets from Hackage

### On Linux systems

- `sudo apt install libxext-dev` (installs Xlib etc on Debian-based
  distributions, the command will be different on other Linux distributions.)
- `cabal install fudgets`

### On macOS

- Install [XQuartz](https://www.xquartz.org/).
- `brew install gcc` (need the version cpp included with gcc, since there
  are some issues with cpp from clang.
  Note: `fudgets.cabal` refers to `cpp-10`, you might need to change this
  if you install a different version of gcc.)
- `cabal install fudgets`

