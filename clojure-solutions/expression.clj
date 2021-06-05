(defn base_expression [f]
  (fn [& args]
    (fn [variables]
      (apply f
             (mapv (fn [elem] (elem variables)) args)
             )
      )
    )
  )

(defn variable [variable]
  (fn [variables] (get variables variable))
  )

(comment ":NOTE: use `constantly` instead")
(def constant constantly)

(def add (base_expression +))

(def subtract (base_expression -))

(def multiply (base_expression *))

(comment ":NOTE: why it's just a binary operator?")
(def divide (base_expression (fn [a & b] (/ (double a) (apply * b)))))

(def negate subtract)

(def avg (base_expression (fn [& args] (/ (apply + args) (count args)))))

(def med (base_expression (fn [& args] (nth (sort args) (/ (count args) 2)))))

(definterface IExpression
  (^Number evaluate [vars])
  (^String toString [])
  (^String toStringInfix [])
  (diff [variable]))

(defn evaluate [args vars] (.evaluate args vars))
(defn toString [args] (.toString args))
(defn toStringInfix [args] (.toStringInfix args))
(defn diff [args var] (.diff args var))

(deftype JBaseOperation [func oper dif args]
  IExpression
  (evaluate [this vars] (apply func (mapv #(evaluate % vars) args)))
  (toString [this] (str "(" oper " " (clojure.string/join " " (map #(toString %) args)) ")"))
  (toStringInfix [this] (do
                          ;(println (type oper))
                          (cond
                            (= oper "negate") (str "negate(" (clojure.string/join (str " " oper " ") (map #(.toStringInfix %) args)) ")")
                            (= (count args) 1) (str oper (clojure.string/join (str " " oper " ") (map #(.toStringInfix %) args)))
                            :else (str "(" (clojure.string/join (str " " oper " ") (map #(toStringInfix %) args)) ")"))))
  (diff [this var] (apply dif var args)))

(defn BaseOperation [func oper dif & args] (JBaseOperation. func oper dif args))

(def Add (partial BaseOperation
                  +
                  "+"
                  (fn [var & args] (apply Add (mapv #(diff % var) args)))))

(def Subtract (partial BaseOperation
                       -
                       "-"
                       (fn [var & args] (apply Subtract (mapv #(diff % var) args)))))

(def Negate (partial BaseOperation
                     -
                     "negate"
                     (fn [var arg] (Negate (diff arg var)))))

(def Multiply (partial BaseOperation
                       *
                       "*"
                       (fn [var & args]
                         (if (= (count args) 1)
                           (diff (first args) var)
                           (Add (apply Multiply (diff (first args) var) (rest args))
                                (Multiply (first args) (diff (apply Multiply (rest args)) var)))))))

(def Divide (partial BaseOperation
                     (fn [a & b] (/ (double a) (apply * b)))
                     "/"
                     (fn [var & args]
                       (Divide
                         (Subtract
                           (apply Multiply (diff (first args) var) (rest args))
                           (Multiply (diff (apply Multiply (rest args)) var) (first args)))
                         (apply Multiply (map #(Multiply % %) (rest args)))))))

(defn pow [b x] (Math/pow b x))

(def Pow (partial BaseOperation
                  pow
                  "**"
                  nil
                  ))

(defn log [x b] (/ (Math/log (Math/abs b)) (Math/log (Math/abs x))))

(def Log (partial BaseOperation
                  log
                  "//"
                  nil
                  ))

(def Sum (partial BaseOperation
                  +
                  "sum"
                  (fn [var & args] (apply Add (mapv #(diff % var) args)))))

(declare Zero)

(deftype JConstant [value]
  IExpression
  (evaluate [this vars] value)
  (toString [this] (format "%.1f" (double value)))
  (toStringInfix [this] (format "%.1f" (double value)))
  (diff [this x] Zero))

(defn Constant [value] (JConstant. value))

(def Avg (partial BaseOperation
                  (fn [& args] (/ (apply + args) (count args)))
                  "avg"
                  (fn [var & args] (diff (Divide (apply Sum args) (Constant (count args))) var))))

(def Zero (Constant 0.0))
(def One (Constant 1.0))

(deftype JVariable [variable_name]
  IExpression
  (evaluate [this vars] (vars variable_name))
  (toString [this] variable_name)
  (toStringInfix [this] variable_name)
  (diff [this var] (if (= var variable_name)
                     One
                     Zero)))

(defn Variable [variable_name] (JVariable. variable_name))


(def operators {'+      add
                '-      subtract
                '*      multiply
                '/      divide
                'negate negate
                'med    med
                'avg    avg})

(defn parseFunction [input]
  (letfn [
          (answer [data]
            (cond
              (number? data) (constant data)
              (symbol? data) (variable (str data))
              :else (apply (operators (first data)) (mapv answer (rest data)))
              )
            )
          ] (answer (read-string input)))
  )

(def objectOperators {'+      Add
                      '-      Subtract
                      '*      Multiply
                      '/      Divide
                      'negate Negate
                      'avg    Avg
                      'sum    Sum})

(defn parseObject [input]
  (letfn [
          (answer [data]
            (cond
              (number? data) (Constant data)
              (symbol? data) (Variable (str data))
              :else (apply (objectOperators (first data)) (mapv answer (rest data)))
              )
            )
          ] (answer (read-string input)))
  )

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
                       "!"))

(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))

(defn _empty [value] (partial -return value))

(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

(defn _map [f result]
  (if (-valid? result)
    (-return (f (-value result)) (-tail result))))

(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        (_map (partial f (-value ar))
              ((force b) (-tail ar)))))))

(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn +char [chars] (_char (set chars)))

(defn +char-not [chars] (_char (comp not (set chars))))

(defn +map [f parser] (comp (partial _map f) parser))

(def +parser _parser)

(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce _either p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

(declare *parseZ)

(def *all-chars (mapv char (range 0 128)))
(def *digit (+char "0123456789."))
(def *number (+map read-string (+str (+plus *digit))))
(defn *string [st] (apply +seqf str (mapv #(+char (str %) )st)))

(def infix
  (let
    [
     *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars)))
     *ws (+ignore (+star *space))
     *letter (+char (apply str (filter #(Character/isLetter %) *all-chars)))
     *variable (+str (+plus *letter))
     *negate (+seqf (constantly 'negate) (*string "negate"))]

    (letfn [
            (*parseI [] (+seqn 0 *ws (+or *number *variable) *ws))
            (*parseF [] (+or (+seq *ws *negate (delay (*parseF)) *ws)
                             (+seqn 0 *ws (+seq (+or (+char "-+") *ws)
                                                (+seqn 0 (+or (*parseI) (+seqn 1 (+char "(") *ws (delay (*parseE)) *ws (+char ")") *ws)))))))
            (*parseP [] (+or (+seq *ws (delay (*parseF)) *ws (+seq (+or (+seqf str (*string "**")) (+seqf str (*string "//"))) *ws (delay (*parseP)) *ws)) (delay (*parseF))))
            (*parseET [str parse] (+seqf cons (delay (parse)) (+star (+seq *ws (+char str) *ws (delay (parse)) *ws))))
            (*parseT [] (*parseET "*/" *parseP))
            (*parseE [] (*parseET "+-" *parseT))
            (*value [] (delay (*parseE)))]
      (+parser (+seqn 0 *ws (*value))))
    )
  )

; a ** b ** c ** d
; (a ** (b ** (c ** d)))

(def objectCharOperators {\+      Add
                          \-      Subtract
                          \*      Multiply
                          \/      Divide
                          "**"    Pow
                          "//"    Log
                          'negate Negate})

(defn foldr [f coll]
  (do
    (if (== (count coll) 1) (first coll)
                            (f (first coll) (foldr f (rest coll))))
    ))

(defn parseObjectInfix [input]
  (letfn [
          (answer [data]
            (do
              (cond
                (number? data) (Constant data)
                (string? data) (Variable data)
                (seq? data) (if (= (count data) 1)
                              (answer (nth data 0))
                              (reduce (fn [x y] (apply (objectCharOperators (nth y 0)) (mapv answer (vector x (nth y 1))))) data))
                (vector? data) (cond
                                 (= (count data) 1) (answer (nth data 0))
                                 (objectCharOperators (nth data 0)) (apply (objectCharOperators (nth data 0)) (mapv answer (vector (nth data 1))))
                                 :else (reduce (fn [x y] (apply (objectCharOperators (nth y 0)) (mapv answer (vector x (nth y 1))))) data)
                                 )
                :else data)))]
    (answer (infix input))))

;(println (toStringInfix (parseObjectInfix "negate 2")))
(println (infix "a + 2"))