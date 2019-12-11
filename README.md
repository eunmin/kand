# Kand

This is experimental project. Don't use it for product. :)
 
## Usage

You can run repl with [Leiningen](https://leiningen.org) `lein run`

```
Kand REPL
To exit type quit
> (def a 10)
#kand.type.Unit{}
> (def b 2000)
#kand.type.Unit{}
> (+ a b)
#kand.type.Num{:val 2010}
> (def add (fn (x y) (+ x y)))
#kand.type.Unit{}
> (add a b)
#kand.type.Num{:val 2010}
> (if (> 1 2) 1 2)
#kand.type.Num{:val 2}
> (if (= 1 1) 1 2)                             
#kand.type.Num{:val 1}
> (if (= 1 2) 1 2)
#kand.type.Num{:val 2}
> (. (quote nextInt) (new (quote java.util.Random)))
#kand.type.Obj{:obj -1034235661}
> :quit
Bye See you soon!
```

## Build

```
lein ubarjar

java -jar target/kand-0.1.0-SNAPSHOT-standalone.jar
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

### Tools

- [ASM Bytecode Viewer](https://plugins.jetbrains.com/plugin/10302-asm-bytecode-viewer)

### Books

- [Thinking with Types](https://leanpub.com/thinking-with-types)

### Articles

- [CLOJURE VS. THE STATIC TYPING WORLD](https://lispcast.com/clojure-and-types/)

### Friend Projects

- [lengine](https://github.com/gkm2164/lengine)
