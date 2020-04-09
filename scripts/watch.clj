(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'kidoku-web.core
   :output-to "out/kidoku_web.js"
   :output-dir "out"})
