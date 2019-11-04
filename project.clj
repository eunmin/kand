(defproject kand "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.apache.commons/commons-lang3 "3.9"]
                 [org.ow2.asm/asm "7.0"]
                 [org.ow2.asm/asm-util "7.0"]]
  :repl-options {:init-ns kand.main}
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.10.0"]
                                  [expound "0.7.2"]]}}
  :main kand.main)
