<center>
<h1>cyn</h1>
Shell language with haskell-ish syntax.
</center>

```
  ....  .... ... .. ...
.|   ''  '|.  |   ||  ||
||        '|.|    ||  ||
 '|...'    '|    .||. ||.
        .. |
         ''
-------- v0.1.0 --------
C:\Users\jaspe\dev\cyn >> add a b = $a + $b
C:\Users\jaspe\dev\cyn >> add 1
λb -> ($a + $b)
C:\Users\jaspe\dev\cyn >> increment = add 1
C:\Users\jaspe\dev\cyn >> increment 4
5
C:\Users\jaspe\dev\cyn >> ls
src/
target/
Cargo.lock
Cargo.toml
LICENSE
README.md
C:\Users\jaspe\dev\cyn >> std.filter (λs -> len $s > 9) (ls --array)
[.gitignore, Cargo.lock, Cargo.toml]
C:\Users\jaspe\dev\cyn >> README_ENV_VAR $= cat README.md
C:\Users\jaspe\dev\cyn >> "gc $msg = git add -A; git commit -a -m $msg; git push" >> "C:\Users\jaspe\.cynrc"
```

## Building
```sh
cargo build --release
```
Binary will be at `target/release/cyn`

## Current State

This project is technically usable and I use it as my personal shell but is still just an early concept and lacks stability, various features and the semantics are quite confusing. I plan to work on it more in the future.
