(ns playground.types

(:import (clojure.lang Atom Symbol Namespace Keyword Named IMapEntry Seqable
                         LazySeq PersistentHashSet PersistentTreeSet PersistentList APersistentVector
                         APersistentSet Sorted IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap IDeref ISeq IMeta ASeq IPersistentCollection
                         ILookup Indexed Associative IPersistentStack PersistentVector Cons
                         IPersistentList IRef IReference AReference ARef Var Delay Reversible
                         ITransientCollection ITransientSet ITransientAssociative ITransientMap
                         ITransientVector PersistentHashMap Reduced))
 (:require 
  [clojure.core.typed :as t]
  [clojure.set :as cs]
;; [clojure.core.typed.base-env-helper :as h]
;; [clojure.core.typed.base-env-common :refer [delay-and-cache-env]]
;; [clojure.core.typed.parse-unparse :as prs]
;; [clojure.core.typed.type-rep :as r]
;; [clojure.core.typed.type-ctors :as c]
;; [clojure.core.typed.path-rep :as pe]
;; [clojure.core.typed.object-rep :as obj]
;; [clojure.core.typed.fold-default]
;; [clojure.core.typed.name-env :as nme-env]
;; [clojure.core.typed.subst]
;; [clojure.core.typed.rclass-env :as rcls]
;; [clojure.core.typed.current-impl :as impl :refer [v]]
[clojure.set :as set]))

  

(t/ann ^:no-check union-occ (All [a] (Fn [(t/Set a) * -> (t/Set a)])))
(def union-occ clojure.set/union)

(t/ann ^:no-check union-uni (All [x [x1 :< x :> x]]
                                 (Fn [(t/Set x) (t/Set x1) * -> (t/Set x1)])))
(def union-uni clojure.set/union)

(t/ann ^:no-check union-cov (All [x [x1 :> x]]
                                 (Fn [(t/Set x) (t/Set x1) * -> (t/Set x1)])))
(def union-cov clojure.set/union)


;;; Contravariance of functions
(t/defn> checkia :- Boolean [f :- (Fn [t/Int -> (t/Option Any)])] (not (f 42)))
(t/defn> checkna :- Boolean [f :- (Fn [t/Num -> (t/Option Any)])] (not (f 42)))
(t/defn> checkib :- Boolean [f :- (Fn [t/Int -> Boolean])] (f 42))
(t/defn> checknb :- Boolean [f :- (Fn [t/Num -> Boolean])] (f 42))
(t/defn> checkio :- Boolean [f :- (Fn [t/Int -> (t/Option t/Int)])] (not (f 42)))
(t/defn> checkno :- Boolean [f :- (Fn [t/Num -> (t/Option t/Num)])] (not (f 42)))

(t/defn> f42ib :- Boolean [x :- t/Int] (= x 42))
(t/defn> f42ab :- Boolean [x :- Any] (= x 42))
(t/defn> f42ia :- Any [x :- t/Int] (= x 42))
(t/defn> f42aa :- Any [x :- Any] (= x 42))
(t/defn> f42io :- (t/Option t/Int) [x :- t/Int] (if (= x 42) 42 nil))
(t/defn> f42ao :- (t/Option t/Int) [x :- Any] (if (= x 42) 42 nil))

;;  check  f42  ia aa ib ab  io ao  #{2} (Set Int)  (PHS Int) (PHS Any) (PHS Num)
;;  ia          ok ok ok ok  ok ok   x   x          ok        ok        ok
;;  ib          x  x  ok ok  x  x    x   x          x         x         x
;;  na          x  ok x  ok  x  ok   x   x          OK        ok        ok
;;  nb          x  x  x  ok  x  x    x   x          x         x         x
;;  io          x  x  x  x   ok ok  ok   x          ok        x         x
;;  no          x  x  x  x   x  ok  ok   x          OK        x         ok

;;ok
;(t/cf (checkna f42aa))  
;(t/cf (checkna f42ab))
;(t/cf (checkia anything))
;(t/cf (checkib f42ab))
;(t/cf (checkib f42ib))
;; not ok
;(t/cf (checkna f42ia))
;(t/cf (checkna f42ib))
;(t/cf (checkib f42ia))


;;Cool:
;; (t/cf (checkn f42i)) ; errors
;; (t/cf (checki f42i)) ; doesn't
;; (t/cf (checka f42a)) ; doesn't

(t/defn> inductsi :- (t/Set t/Int) [s :- (t/Set t/Int)] (union-occ s #{42}))


;(t/cf (checkn (t/ann-form #{42} (clojure.lang.PersistentHashSet Any)))) ;; ok

;(t/cf (checkn (t/ann-form #{42} (clojure.lang.PersistentHashSet t/Int))))
;; doesnt error either - sets are functions of any

#_(t/cf ((t/fn> :- Any [f :- [t/Int -> (t/Option t/Int)]] (let [x (f 3)] (if x (inc x) "bleh"))) #{2}))

(t/ann ^:no-check conj-strict (All [x [x1 :< x :> x]] (Fn [(t/Set x) x1 * -> (t/Set x1)])))
(def conj-strict clojure.core/conj)

(t/ann ^:no-check conj-set
       (All [x
             [y :< x]]
            [(t/Set x) y y * -> (t/Set x)]))
(t/ann ^:no-check conj-set-scala
       (All [x
             [y :> x :< x]]
            [(t/Set x) y y * -> (t/Set y)]))
(def conj-set conj)
(def conj-set-scala conj)

(t/ann ^:no-check conj-vec
     (All [x
           [y :< x]]
          [(t/Vec x) y y * -> (t/Vec x)]))
(t/ann ^:no-check conj-vec-scala
     (All [x
           [y :< x]]
          [(t/Vec x) y y * -> (t/Vec x)]))
(def conj-vec conj)
(def conj-vec-scala conj)

#_(h/alters
APersistentSet [[[a :variance :covariant]]
                :replace
                {Seqable (Seqable a)
                 IFn [Any -> (U a nil)]
                 AFn [Any -> (U a nil)]
                 IPersistentCollection (IPersistentCollection 
                                         a
                                         #_(TFn [[x :variance :covariant]]
                                              x)
                                         #_(TFn [[x :variance :covariant]]
                                              (APersistentSet x)))
                 IPersistentSet (IPersistentSet a)}] )

(t/def-alias MMap "bleh" (All [a [a1 :< a :> a]] (t/Map (t/Set a) (t/Vec a1))))

(t/ann blort [(t/Map (t/Set String) (t/Seq t/AnyInteger)) (t/Set String)  (t/Seq t/AnyInteger)  ->  (t/Map (t/Set String) (t/Seq t/AnyInteger)) ]   )

(t/ann blort (All [a [a1 :< a :> a]] (Fn [(t/Map (t/Set a) (t/Vec a1)) (t/Set a1) (t/Vec a1) -> (t/Map (t/Set a1) (t/Vec a1)) ])))

(defn blort [m k v]
  (assoc m k v))
