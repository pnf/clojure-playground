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




(defn idxid [ntk T]
  (let [ntkh  (digest/md5 ntk)
        Ts    (-> T .getTime (as-> % (- 100000000000000 %)) (as-> % (format "%016d" %)))]
    (str ntkh "-" Ts)
    ))

(defn  jd 
  ([T]
     (condp = (type T)
       java.lang.Long (java.util.Date. T)
       java.util.Date T
       java.lang.String (-> T co/from-string co/to-date)

))
  ([] (java.util.Date.))

)

(defn insert-value [conn ntk T value] 
  (let [T (jd T)
        idx (idxid ntk T)
        prev (seq  (q `[:find ?id :where [?id :bitemp/index ~idx]] (db conn)))
        eid (if prev (ffirst prev) (d/tempid :db.part/user))]
    @(d/transact conn [{:db/id eid
                        :bitemp/index idx
                        :bitemp/ntk ntk
                        :bitemp/T T
                        :bitemp/value value}])))

(defn print-hist
  ([conn]
     (doseq [t  (sort (q '[:find ?when :where [_ :db/txInstant ?when]] (db conn)))]
       (let [tx (first t)
             r  (q '[:find ?e ?ntk ?T ?i ?v :where [?e :bitemp/ntk ?ntk] [?e :bitemp/T ?T] [?e :bitemp/index ?i] [?e :bitemp/value ?v] ] (-> conn db (d/as-of tx)))
             ]
         (println t (count  r) r))))
  ([conn ntk T]
     (let [idx  (idxid ntk T)
           id   (ffirst (q `[:find ?id :where
                           [?id :bitemp/index ~idx]] (db conn)))
           _ (println id)
           txs  (q `[:find ?t ?a ?v ?tx ?added
                     :where [~id ?a ?v ?tx ?added] [?tx :db/txInstant ?t]]
                   (d/history (db conn)))]
       (doseq [tx txs] (println tx)))))



(defn get-at-vt [conn ntk T]
  (let [idx (idxid ntk T)
        ds  (seq (d/seek-datoms (db conn) :avet :bitemp/index idx))]
    (and ds (.v (first ds)))
))

(defn get-at-bt [conn ntk T t]
  (let [dbt (-> conn db (d/as-of t))
        idx (idxid ntk T)
        ds  (seq (d/seek-datoms dbt :avet :bitemp/index idx))]
    (and ds (q `[:find ?T ?v :where
                 [?e :bitemp/index ~(.v (first ds))]
                 [?e :bitemp/value ?v]
                 [?e :bitemp/T ?T]] dbt))))


(comment 

  #_(defn insert-values [nKeys nTimes]

      )


  #_(def tx-instants (sort (q '[:find ?when :where [_ :db/txInstant ?when]] (db conn))))

  #_(q '[:find ?e ?ntk ?T ?i :where [?e :bitemp/ntk ?ntk] [?e :bitemp/T ?T] [?e :bitemp/index ?i]] (db conn))

                                        ; Number of entities over time
  
  )
