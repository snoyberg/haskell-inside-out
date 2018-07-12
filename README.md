# haskell-inside-out

Welcome to the Haskell Inside Out workshop material! There are some
exercises in this repo to go along with the talk. If you're going to
attend the talk, I recommend getting your machine set up in advance,
since downloading the tools can take a bit of time on a slow
conference connection.

You'll essentially need three things for this workshop:

1. The repo itself, which can be cloned with

   ```
   git clone https://github.com/snoyberg/haskell-inside-out
   ```

   Note: I will be making changes to this repo up until the talk
   itself, so if you cloned in advance, run a `git pull` at the start
   of the talk.

2. The Stack build tool, which can install a Haskell toolchain and
   install packages.

   * On Windows, please [use the installer](https://www.stackage.org/stack/windows-x86_64-installer).
   * On Linux and OS X, run `curl -sSL https://get.haskellstack.org/ | sh`

   More information is
   [available on the Get Started page](https://haskell-lang.org/get-started).

3. Setup the compiler by running `stack build` inside this directory.

I'll provide instructions during the workshop of how to perform the
exercises.
