# backend-api

## Testing

Cabal run has better colored output

```sh
cabal run spec
```

Optionally you can rerun failed tests only. See <http://hspec.github.io/rerun.html>.

```sh
echo --failure-report .hspec-failures >> ~/.hspec
echo --rerun >> ~/.hspec
echo --rerun-all-on-success >> ~/.hspec
```

If your `cabal.project.local` is configured right, a file called `spec.tix` is generated.
Use `./scripts/test-report.sh` to generate the `hpc` Code Coverage report.
Ideally the test-suite has good coverage.

Build the documentation

```sh
cabal haddock
```

A reference to the generated `index.html` file is show by cabal. Open this file
in a browser to check if the generated documentation is correct.

### Hoogle local code

You can only create a database for your own code
(see <https://github.com/ndmitchell/hoogle/issues/288>) as follows

```sh
hoogle generate --local=. --database local.hoo
```

Then you can serve this in a browser

```sh
hoogle server --database local.hoo
```

You might have to replace the dot in `--local=.` by path outputted by
`$ cabal haddock --haddock-hoogle`.
For other dependencies I recommend using an online Hoogle database.

### Profiling your app

See the Real World Haskell book
(<http://book.realworldhaskell.org/read/profiling-and-optimization.html>)
for a great introduction on how to spot performance issues in your program.
Here is a TL;DR. Run the program or tests with statistic reporting enabled.
This is done by passing the argument `-sstderr` to the RunTime System (RTS).

```sh
cabal run spec -O2 -- +RTS -sstderr
```

If the program spends much time Garbage Collecting (GC) your program probably
has a space leak. See the Real World Haskell book for more info how to solve this.
To get a detailed report of how much time the program spends on each of the
top level functions, run the following.

```sh
cabal run spec -O2 --enable-profiling -fprof -- +RTS -p
```

Both `--enable-profiling` and `-fprof` are for compiling the program for generating
a profiling report.
Then we need to pass to the RTS that we actually want such a report.
Add either `-hc`, `-hy` or `-hd` for call, type or data based heap profiling.
A `.hp` file is generated. You can visualize this data with

```sh
hp2ps -e8in -c <filename>.hp
```

Thats it, now you are able to start figuring out where your program spends most of
its time and space.
