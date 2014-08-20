(defn context [expr]
  (if (seq? expr)
    (let [operator (first expr)
          args (rest expr)]
      {:operator operator :context (list operator :hole) :term args})))

(defn immediately-reducible? [expr]
  (let [{operator :operator args :term} (context expr)]
    (and (or
          (= operator '+)
          (= operator +))
         (every? number? args))))

(defn terminal? [expr]
  (number? expr))

(defn reduce-expr [expr]
  (cond (terminal? expr) expr
        (immediately-reducible? expr) (apply + (rest expr))
        true `(~'+ ~@(map reduce-expr (rest expr)))))

(defn evaluate-fully [expr]
  (loop [expr expr
         steps [expr]]
    (let [new-expr (reduce-expr expr)]
      (if (= new-expr expr)
        steps
        (recur new-expr (conj steps new-expr))))))

(def test-vals [
  1
  2
  '(+ 1 2 3)
  '(+ (+ 1 2 3) 4)])

(map immediately-reducible? test-vals)
(map context test-vals)
(map terminal? test-vals)
(reduce-expr (test-vals 2))
(reduce-expr (test-vals 3))
(evaluate-fully (test-vals 3))
