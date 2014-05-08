(ns playground.bitemp
  (:use [datomic.api :only [q db] :as d]
        [clojure.pprint])
  (:require  digest
             [clj-time.core :as ct]
             [clj-time.coerce :as co]
             [clj-time.format :as cf]))

(def uri "datomic:free://localhost:4334/bitemp")

#_(def conn (d/connect uri))

(def schema-tx 
    [{:db/id #db/id[:db.part/db]
      :db/ident :bitemp/ntk
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one
      :db/doc "Non-temporal key"
      :db.install/_attribute :db.part/db}

     {:db/id #db/id[:db.part/db]
     :db/ident :bitemp/T
     :db/valueType :db.type/instant
     :db/cardinality :db.cardinality/one
     :db/doc "Time for which data is relevant"
     :db/index true
     :db.install/_attribute :db.part/db}

     {:db/id #db/id[:db.part/db]
      :db/ident :bitemp/index
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one
      :db/doc "concatenated ndtid and instantmsec"
      :db/unique :db.unique/identity
      :db.install/_attribute :db.part/db}

     {:db/id #db/id[:db.part/db]
      :db/ident :bitemp/value
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one
      :db/doc "The value"
      :db.install/_attribute :db.part/db}
     ]

)

(defn recreate-db []
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)]
    @(d/sync conn)
    @(d/transact conn schema-tx)
    @(d/transact conn [{:db/id (d/tempid :db.part/db)
                        :db/ident :bitemp 
                        :db.install/_partition :db.part/db}])
    conn))

(defn connect []
  (let [conn (d/connect uri)]
    @(sync conn)
    conn))


(defn k-hash [k] (digest/md5 k))
(defn t-format [t] (-> t .getTime (as-> % (- 100000000000000 %)) (as-> % (format "%016d" %))))
(def idx-sep "-")
(defn idxid [k T] (str (k-hash k) idx-sep (t-format T)))

(defn  jd 
  ([T]
     (condp = (type T)
       java.lang.Long (java.util.Date. T)
       java.lang.Integer (java.util.Date. (long T))
       java.util.Date T
       java.lang.String (-> T co/from-string co/to-date)))
  ([] (java.util.Date.)))


#_(defn insert-value [conn ntk T value] 
  (let [T (jd T)
        idx (idxid ntk T)
        prev (seq  (q `[:find ?id :where [?id :bitemp/index ~idx]] (db conn)))
        eid (if prev (ffirst prev) (d/tempid :db.part/user))]
    @(d/transact conn [{:db/id eid
                        :bitemp/index idx
                        :bitemp/ntk ntk
                        :bitemp/T T
                        :bitemp/value value}])))

(defn insert-tx [ntk T value] 
  (let [T (jd T)
      idx (idxid ntk T)]
    {:db/id (d/tempid :bitemp) ;#db/id[:user.part/users]
                          :bitemp/index idx
                          :bitemp/ntk ntk
                          :bitemp/T T
                          :bitemp/value value}))

; implicit upsert seems to be just as slow
(defn insert-value [conn ntk T value] 
  "Insert a value and return time of insert"
  (-> @(d/transact conn [(insert-tx ntk T value)])
      :tx-data first  .v))

(defn insert-values [conn ntk-T-values] 
  "Insert a value and return time of insert"
  (-> @(d/transact conn (map (partial apply insert-tx) ntk-T-values))
      :tx-data first  .v))


(defn print-hist
  ([conn]
     (doseq [t  (sort (q '[:find ?when :where [_ :db/txInstant ?when]] (db conn)))]
       (let [tx (first t)
             r  (q '[:find ?e ?ntk ?T ?i ?v :where [?e :bitemp/ntk ?ntk] [?e :bitemp/T ?T] [?e :bitemp/index ?i] [?e :bitemp/value ?v] ] (-> conn db (d/as-of tx)))
             ]
         (println t (count  r) r))))
  ([conn ntk T]
     (let [a    (:id (d/attribute (db conn) :bitemp/value))
           _    (println a)
           idx  (idxid ntk T)
           id   (ffirst (q `[:find ?id :where
                           [?id :bitemp/index ~idx]] (db conn)))
           txs  (sort-by first (q `[:find ?t ?tx ?v
                       :where [~id ~a ?v ?tx true]
                              [?tx :db/txInstant ?t]]
                     (d/history (db conn))))]
       (doseq [tx txs] (println tx)))))


#_(let [db (db conn)]
    (q '[:find ?v ?i
         :in $ [[?ts]] ?k
         :where
         [?ts :bitemp/index ?i]
         [?ts :bitemp/ntk ?k]
         [?ts :bitemp/value ?v]]
       db
       (seq (d/index-range db :bitemp/T (jd 10) (jd 30))) "Thing4"))

#_(seq (d/index-range (db conn)
                      :bitemp/index
                      (idxid "Thing4" (jd 20))
                      (idxid "Thing4" (jd 10))))

(defn get-at [conn k T & t]
  (let [Tf  (t-format (jd T))
        kh  (k-hash k)
        idx (str kh idx-sep Tf)
        db  (if t (-> conn db (d/as-of (first  t))) (db conn))
        es  (some-> (d/index-range db :bitemp/index idx nil)
                    seq
                    first
                    .v
                    )]
     (and es
         (.startsWith es kh)
         (first (q '[:find ?T ?v
                     :in $ ?i ?k
                     :where
                     [?e :bitemp/index ?i]
                     [?e :bitemp/ntk   ?k]
                     [?e :bitemp/value ?v]
                     [?e :bitemp/T     ?T]
                     ]
                   db es k)))
))


(defn dbtime [conn] 
  (let [db (db conn)]
      (-> (d/entity db (d/t->tx (d/basis-t db))) :db/txInstant)))


(defn insert-lots [conn nKeys nTv nRev nKeyBatch]
  (let [txs (for [r  (range nRev)
                  T  (range nTv)
                  k0 (range 0 nKeys nKeyBatch)
                  :let [k1 (+ k0 nKeyBatch)]]
              (do 
                (println "inserting between" k0 k1)
                (insert-values conn
                               (for [k (range k0 k1)]
                                 [(str "k" k "v" T "r" r)
                                  (* 10 T)
                                  (str "Thing" k)
                                  ]
                                 ))))]
    [(first txs) (last txs)]))

(defn query-lots [conn nKeys nTv [t1 t2] n]
  (let [t1 (.getTime t1)
        dt (- (.getTime t2) t1)]
    (dotimes [_ n]
      (let [T (* 10  (rand-int nTv))
            t (jd (+ t1 (rand-int dt)))
            k (str "Thing" (rand-int nKeys))
            v (get-at conn k T)]
        ;(println "Querying" k T t v)
        ))))


(comment 


  #_(def tx-instants (sort (q '[:find ?when :where [_ :db/txInstant ?when]] (db conn))))

  #_(q '[:find ?e ?ntk ?T ?i :where [?e :bitemp/ntk ?ntk] [?e :bitemp/T ?T] [?e :bitemp/index ?i]] (db conn))

                                        ; Number of entities over time
  
  )
