(ns anamorphic-adventure.core)


;; copied and c
(defn unfold
  ([p f g seed tail-g]
   (lazy-seq
     (if (p seed)
       (tail-g seed)
       (cons (f seed)
             (unfold p f g (g seed) tail-g)))))
  ([p f g seed]
     (unfold p f g seed (fn [_] ()))))

;; unfoldr :: (b -> Maybe (a,b)) -> b -> [a]

(defn unfold2
  "g is a function which takes seed and returns [g' seed'] or nil
   function returns seq of g's"
  [g seed]

  (lazy-seq
   (let [[a b] (g seed)]
     (if (identity a)
      (cons a
            (unfold g b))
      nil)))
  )

(defn toBinary2 [x]
  (let [g (fn [n]
            (if (= n 0)
              nil
              [(mod n 2) (int (/ n 2))]))]
    (reverse (unfold2 g x))))



;;(take 1 (toBinary2 16))

(defn unfold
  "g is a function which takes seed and returns [g' seed'] or nil
   function returns seq of g's"
  [g seed]

  (lazy-seq
   (let [[a b] (g seed)]
     (cons a
           (unfold g b))))
  )


(defn toBinary [x]
  (let [g (fn [n]
            (if (= n 0)
              nil
              [(mod n 2) (int (/ n 2))]))]
    (reverse (take-while identity (unfold g x)))))



(toBinary 16)

(defmacro letrec [bindings & body]
  (let [bcnt (quot (count bindings) 2)
        arrs (gensym "bindings_array")
        arrv `(make-array Object ~bcnt)
        bprs (partition 2 bindings)
        bssl (map first bprs)
        bsss (set bssl)
        bexs (map second bprs)
        arrm (zipmap bssl (range bcnt))
        btes (map #(clojure.walk/prewalk (fn [f]
                                   (if (bsss f)
                                     `(aget ~arrs ~(arrm f))
                                     f))
                                 %)
                  bexs)]
    `(let [~arrs ~arrv]
       ~@(map (fn [s e]
                `(aset ~arrs ~(arrm s) ~e))
              bssl
              btes)
       (let [~@(mapcat (fn [s]
                         [s `(aget ~arrs ~(arrm s))])
                       bssl)]
         ~@body))))

(defn unfold-nat-old [g seed]
  (letrec [seeds (cons seed
                       (map (comp second g) (rest seeds)))]
          (map (comp first g) seeds)
          seeds))

(defn unfold-nat [g seed]
  (->> (g seed)
       (iterate (comp g second))
       (take-while identity)
       (map first)))



(defn to-binary-nat [x]
  (let [g (fn [n]
            (if-not (zero? n)
              [(mod n 2) (int (/ n 2))]))]
    (reverse (unfold-nat g x))))

(defn to-binary-nat2 [x]
  (reverse (unfold-nat
            (fn [n]
              (if-not (zero? n)
                [(mod n 2) (int (/ n 2))]))
            x)))


;;(to-binary-nat2 16)


(defn map-accum
  "Analogous to Haskell mapAccumL
   f is a function which takes 2 parameters: acc and x and returns [acc' x']
   returns [acc' coll']"
  [f acc coll]
  (reduce (fn [[acc res] x] (update-in (f acc x) [1] (partial conj res))) [acc []] coll))
