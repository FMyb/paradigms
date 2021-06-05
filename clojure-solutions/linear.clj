(defn eq_sz? [elements]
  (every? (fn [element] (== (count (first elements)) (count element))) elements))

(defn many_arg [func]
  (fn [& items]
    {
     :pre [(eq_sz? items)]
     }
    (apply mapv func items)
    )
  )

;(defn v+ [& items]
;  (many_arg + items)
;  )

(def v+ (many_arg +))

(def v* (many_arg *))

(def v- (many_arg -))

(defn scalar [& items]
  (apply + (apply v* items)))

(defn vect [& items]
  {
   :pre [(every? (fn [x] (== (count x) 3)) items)]
   }
  (letfn [(func [v1, v2]
            (letfn [(oper [ind1, ind2]
                      (- (* (nth v1 ind1) (nth v2 ind2)) (* (nth v1 ind2) (nth v2 ind1))))]
              [(oper 1 2), (oper 2 0), (oper 0 1)])
            )
          ]
    (reduce func items)))

(defn v*s [v & s]
  (let [prod (apply * s)]
    (mapv (fn [x] (* x prod)) v)))

(def m+ (many_arg v+))

(def m- (many_arg v-))

(def m* (many_arg v*))

(comment ":NOTE: same as common 3")
(defn m*s [m & s]
		(let [prod (apply * s)]
		(mapv (fn [x] (v*s x prod)) m)))

(defn m*v [m & v]
  (mapv (fn [vec] (apply scalar vec v)) m)
  )

(defn transpose [m]
  (apply mapv vector m)
  )

(defn m*m [& items]
  (reduce (fn [m1, m2]
            (let [
                  m2t (transpose m2)
                  ]
              (mapv (fn [vec] (m*v m2t vec)) m1)
              )
            ) items
          )
  )

(defn rec [f & tens]
  {:pre [(or (number? (first tens)) (eq_sz? tens))]}
  (if (vector? (first tens))
    (apply mapv (partial rec f) tens)
    (apply f tens)))

(defn checkTensor [tensor]
  (if (every? number? tensor)
    true
    (if (not-every? vector? tensor)
      false
      (if (eq_sz? tensor)
        (every? checkTensor (apply mapv vector tensor))
        false))))

(defn many_arg_tensor [f]
  (fn [& items]
    {:pre [(checkTensor (first items))]}
    (apply rec f items))
  )

(comment "common 1, copy-pase of `partial many_arg_tensor`")
(def t+ (many_arg_tensor +))

(def t- (many_arg_tensor -))

(def t* (many_arg_tensor *))

;(print (t+ (vector (vector 9.4 -2.8) (vector -1.9 -1.0)) (vector (vector 9.4 -2.8) (vector -1.9 -1.0))))
;(print (scalar (vector 1 2 3) (vector 1 2 3)))