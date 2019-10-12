# Kand

## Usage

You can run repl with [Stack](https://docs.haskellstack.org/en/stable/README/) `stack run` or [Etlas](https://eta-lang.org/docs/user-guides/etlas-user-guide) `etlas run`

```
Kand REPL
To exit type quit
> (def a 10)
Unit
> (def b 2000)
Unit
> (+ a b)
Number 2010.0
> (def add (fn (x y) (+ x y)))
Unit
> (add a b)
Number 2010.0
> (if (> 1 2) 1 2)
Number 2.0
> (if (== 1 1) 1 2)
Number 1.0
> (if (== 11 1) 1 2)
Number 2.0
> quit  
Bye See you soon!
```


## References

### JVM Languages

- [Clojure](https://github.com/clojure/clojure/)
- [Scala](https://github.com/scala/scala)
- [Kotlin](https://github.com/JetBrains/kotlin)
- [Eta](https://github.com/typelead/eta)

### Statically typed lisp implementations

- [Lux](https://github.com/LuxLang/lux) (clojure)
- [Carp](https://github.com/carp-lang/Carp) (haskell)
- [Clojure core/typed](https://github.com/clojure/core.typed) (clojure)
- [Typed Racket](https://docs.racket-lang.org/ts-guide/)
- [Shen](http://www.shenlanguage.org)

### Lisp implementations

- [joker](https://github.com/candid82/joker) (clojure)
- [Kawa](https://gitlab.com/kashell/Kawa) (java)
- [little lisp](https://github.com/maryrosecook/littlelisp) (javascript)
- [How to Write a (Lisp) Interpreter (in Python)](http://norvig.com/lispy.html) (python)
- [Lispreter](https://github.com/AoHRuthless/Lispreter) (java)
- [haskell-lisp-interpreter](https://github.com/IvanIvanov/haskell-lisp-interpreter) (haskell)
- [SICP lisp evaluator](https://github.com/eunmin/sicp-ch4/blob/master/src/sicp_ch4/core.clj) (clojure)

### Libraries

- [ASM](https://asm.ow2.io)
