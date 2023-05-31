(ns test.parented-series
  (:require
   [clojure.test :as t :refer [is deftest]]
   [datascript.core :as ds]
   [sy-datascript.core :as sds :refer [transact!]]))

(deftest new-item-in-parented-series-test
  (let [*db (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                             :parent/child {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many}
                             :child/uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:parent/uuid "p1"}
                    {:parent/uuid "p2"}])
    (is (= 1 (get-attr :db/id [:parent/uuid "p1"])))
    (is (= 2 (get-attr :db/id [:parent/uuid "p2"])))
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     {:child/uuid "c1" :child/data :foo}]])
    (is (= 3 (get-attr :db/id [:child/uuid "c1"])))
    (is (= 0 (get-attr :i [:child/uuid "c1"])))

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     {:child/uuid "c2" :child/data :foo}]])
    (is (= 4 (get-attr :db/id [:child/uuid "c2"])))
    (is (= 1 (get-attr :i [:child/uuid "c2"])))

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     {:child/uuid "ch1" :child/data :foo}]])
    (is (= 5 (get-attr :db/id [:child/uuid "ch1"])))
    (is (= 0 (get-attr :i [:child/uuid "ch1"])))
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     {:child/uuid "ch2" :child/data :foo}]])
    (is (= 6 (get-attr :db/id [:child/uuid "ch2"])))
    (is (= 1 (get-attr :i [:child/uuid "ch2"])))

    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))
    nil))

(deftest reorder-item-in-parented-series-test
  (let [*db (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                             :parent/child {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many}
                             :child/uuid {:db/unique :db.unique/identity}})
        check! (fn [[lr a v]]
                 (is (= v (get (ds/pull @*db [a] lr) a))))
        check-all! (fn [tests]
                     (doall (map check! tests)))]
    (transact! *db [{:parent/uuid "p1"}
                    {:parent/uuid "p2"}])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     {:child/uuid "c1" :child/data :foo}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     {:child/uuid "c2" :child/data :foo}]])

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     {:child/uuid "ch1" :child/data :foo}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     {:child/uuid "ch2" :child/data :foo}]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c1"] :i 0]
                 [[:child/uuid "c2"] :i 1]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]])

    (transact! *db [[:sy/reorder-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i "c1" 1]])
    (check-all! [[[:child/uuid "c1"] :i 1]
                 [[:child/uuid "c2"] :i 0]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]])

    (transact! *db [[:sy/reorder-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "ch1" 1]])
    (check-all! [[[:child/uuid "c1"] :i 1]
                 [[:child/uuid "c2"] :i 0]
                 [[:child/uuid "ch1"] :i 1]
                 [[:child/uuid "ch2"] :i 0]])

    (transact! *db [[:sy/reorder-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i "c1" 0]
                    [:sy/reorder-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "ch1" 0]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c1"] :i 0]
                 [[:child/uuid "c2"] :i 1]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]])

    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))
    nil))

(deftest delete-item-in-parented-series-test
  (let [*db (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                             :parent/child {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many}
                             :child/uuid {:db/unique :db.unique/identity}})
        check! (fn [[lr a v]]
                 (is (= v (get (ds/pull @*db [a] lr) a))))
        check-all! (fn [tests]
                     (doall (map check! tests)))]

    (transact! *db [{:parent/uuid "p1"}
                    {:parent/uuid "p2"}])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     {:child/uuid "c1" :child/data :foo}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     {:child/uuid "c2" :child/data :foo}]])

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     {:child/uuid "ch1" :child/data :foo}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     {:child/uuid "ch2" :child/data :foo}]])

    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c1"] :i 0]
                 [[:child/uuid "c2"] :i 1]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]])

    (transact! *db [[:sy/delete-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i "c1"]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c2"] :i 0]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]])

    (transact! *db [[:sy/delete-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "ch2"]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c2"] :i 0]
                 [[:child/uuid "ch1"] :i 0]])

    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))
    nil))

(deftest migrate-item-across-parented-series-test
  (let [schema {:parent/uuid {:db/unique :db.unique/identity}
                :parent/child {:db/valueType :db.type/ref
                               :db/cardinality :db.cardinality/many}
                :child/uuid {:db/unique :db.unique/identity}}
        *save (ds/create-conn schema)
        *db (ds/create-conn schema)
        check! (fn [[lr a v]]
                 (let [correct-v v
                       actual-v (get (ds/pull @*db [a] lr) a)]
                   (when-not (= correct-v actual-v)
                     (print (pr-str [lr a v]))
                     (is (= correct-v actual-v)))))
        check-all! (fn [tests] (doall (map check! tests)))]
    (transact! *db [{:parent/uuid "p1"} {:parent/uuid "p2"}])

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child :child/uuid :i
                     {:child/uuid "a1" :child/data "a1"}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child :child/uuid :i
                     {:child/uuid "a2" :child/data "a1"}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child :child/uuid :i
                     {:child/uuid "a3" :child/data "a3"}]])

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child :child/uuid :i
                     {:child/uuid "b1" :child/data "b1"}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child :child/uuid :i
                     {:child/uuid "b2" :child/data "b1"}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child :child/uuid :i
                     {:child/uuid "b3" :child/data "b3"}]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "a1"] :i 0]
                 [[:child/uuid "a2"] :i 1]
                 [[:child/uuid "a3"] :i 2]
                 [[:child/uuid "b1"] :i 0]
                 [[:child/uuid "b2"] :i 1]
                 [[:child/uuid "b3"] :i 2]])
    (ds/reset-conn! *save (ds/db *db))
    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))

    (transact! *db [[:sy/migrate-item-across-parented-series
                     [:parent/uuid "p1"] [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "a1" 0]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]

                 [[:child/uuid "a2"] :i 0]
                 [[:child/uuid "a3"] :i 1]

                 [[:child/uuid "a1"] :i 0]
                 [[:child/uuid "b1"] :i 1]
                 [[:child/uuid "b2"] :i 2]
                 [[:child/uuid "b3"] :i 3]])
    (ds/reset-conn! *db (ds/db *save))

    (transact! *db [[:sy/migrate-item-across-parented-series
                     [:parent/uuid "p1"] [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "a1" 1]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]

                 [[:child/uuid "a2"] :i 0]
                 [[:child/uuid "a3"] :i 1]

                 [[:child/uuid "b1"] :i 0]
                 [[:child/uuid "a1"] :i 1]
                 [[:child/uuid "b2"] :i 2]
                 [[:child/uuid "b3"] :i 3]])
    (ds/reset-conn! *db (ds/db *save))

    (transact! *db [[:sy/migrate-item-across-parented-series
                     [:parent/uuid "p1"] [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "a1" 2]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]

                 [[:child/uuid "a2"] :i 0]
                 [[:child/uuid "a3"] :i 1]

                 [[:child/uuid "b1"] :i 0]
                 [[:child/uuid "b2"] :i 1]
                 [[:child/uuid "a1"] :i 2]
                 [[:child/uuid "b3"] :i 3]])
    (ds/reset-conn! *db (ds/db *save))

    (transact! *db [[:sy/migrate-item-across-parented-series
                     [:parent/uuid "p1"] [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "a1" 3]])
    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]

                 [[:child/uuid "a2"] :i 0]
                 [[:child/uuid "a3"] :i 1]

                 [[:child/uuid "b1"] :i 0]
                 [[:child/uuid "b2"] :i 1]
                 [[:child/uuid "b3"] :i 2]
                 [[:child/uuid "a1"] :i 3]])
    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))
    (ds/reset-conn! *db (ds/db *save))

    nil))

(deftest remove-item-in-parented-series-test
  (let [*db (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                             :parent/child {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many}
                             :child/uuid {:db/unique :db.unique/identity}})
        check! (fn [[lr a v]]
                 (is (= v (-> (ds/pull @*db [a] lr)
                              (get a)))))
        check-all! (fn [tests]
                     (doall (map check! tests)))]

    (transact! *db [{:parent/uuid "p1"}
                    {:parent/uuid "p2"}])

    (letfn [(add! [parent-id child-id]
              (transact! *db [[:sy/new-item-in-parented-series
                               [:parent/uuid parent-id] :parent/child
                               :child/uuid :i
                               {:child/uuid child-id :child/data :foo}]]))]
      (add! "p1" "c1")
      (add! "p1" "c2")
      (add! "p1" "c3")

      (add! "p2" "ch1")
      (add! "p2" "ch2")
      (add! "p2" "ch3"))

    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c1"] :i 0]
                 [[:child/uuid "c2"] :i 1]
                 [[:child/uuid "c3"] :i 2]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]
                 [[:child/uuid "ch3"] :i 2]])

    (transact! *db [[:sy/remove-item-in-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i "c1"]])

    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))

    (check-all! [[[:child/uuid "c1"] :i nil]
                 [[:child/uuid "c2"] :i 0]
                 [[:child/uuid "c3"] :i 1]])

    (transact! *db [[:sy/remove-item-in-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i "ch2"]])

    (check-all! [[[:child/uuid "c1"] :i nil]
                 [[:child/uuid "c2"] :i 0]
                 [[:child/uuid "c3"] :i 1]

                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i nil]
                 [[:child/uuid "ch3"] :i 1]

                 [[:parent/uuid "p1"] :parent/child [{:db/id 4} {:db/id 5}]]
                 [[:parent/uuid "p2"] :parent/child [{:db/id 6} {:db/id 8}]]])

    nil))

(deftest insert-item-into-parented-series-test
  (let [*db (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                             :parent/child {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many}
                             :child/uuid {:db/unique :db.unique/identity}})
        check! (fn [[lr a v]]
                 (is (= v (-> (ds/pull @*db '[*] lr)
                              (get a)))
                     (pr-str lr a v)))
        check-all! (fn [tests]
                     (doall (map check! tests)))

        ;; print! (fn [] (doall (map #(prn %) (ds/datoms @*db :eavt))))
        ]

    (transact! *db [{:parent/uuid "p1"}
                    {:parent/uuid "p2"}
                    {:parent/uuid "p3"}
                    {:parent/uuid "p-empty"}
                    {:parent/uuid "p-empty2"}])

    (letfn [(add! [parent-id child-id]
              (transact! *db [[:sy/new-item-in-parented-series
                               [:parent/uuid parent-id] :parent/child
                               :child/uuid :i
                               {:child/uuid child-id :child/data :foo}]]))]
      (add! "p1" "c1")
      (add! "p1" "c2")
      (add! "p1" "c3")

      (add! "p2" "ch1")
      (add! "p2" "ch2")
      (add! "p2" "ch3"))

    (check-all! [[[:parent/uuid "p1"] :db/id 1]
                 [[:parent/uuid "p2"] :db/id 2]
                 [[:child/uuid "c1"] :i 0]
                 [[:child/uuid "c2"] :i 1]
                 [[:child/uuid "c3"] :i 2]
                 [[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]
                 [[:child/uuid "ch3"] :i 2]])

    (transact! *db [{:child/uuid "c-empty"}])
    (transact! *db [[:sy/insert-item-into-parented-series
                     [:parent/uuid "p-empty"] :parent/child
                     :child/uuid :j
                     "c-empty" 0]])

    (transact! *db [{:child/uuid "c-empty2"}])
    (transact! *db [[:sy/insert-item-into-parented-series
                     [:parent/uuid "p-empty"] :parent/child
                     :child/uuid :j
                     "c-empty2" 100]])

    ; (print!)
    (check-all! [[[:child/uuid "c-empty"] :j 0]
                 [[:child/uuid "c-empty2"] :j 1]])

    ;;;;;;;;;;;;;;;;;;;;

    (transact! *db [{:child/uuid "ca"}])
    (transact! *db [[:sy/insert-item-into-parented-series
                     [:parent/uuid "p1"] :parent/child
                     :child/uuid :i
                     "ca" 0]])

    (check-all! [[[:child/uuid "ca"] :i 0]
                 [[:child/uuid "c1"] :i 1]
                 [[:child/uuid "c2"] :i 2]
                 [[:child/uuid "c3"] :i 3]])

    ;;;;;;;;;;;;;;;;;;;;

    (transact! *db [{:child/uuid "cha"}])
    (transact! *db [[:sy/insert-item-into-parented-series
                     [:parent/uuid "p2"] :parent/child
                     :child/uuid :i
                     "cha" 3]])

    (check-all! [[[:child/uuid "ch1"] :i 0]
                 [[:child/uuid "ch2"] :i 1]
                 [[:child/uuid "ch3"] :i 2]
                 [[:child/uuid "cha"] :i 3]

                 [[:parent/uuid "p2"] :parent/child
                  [{:db/id 9}
                   {:db/id 10}
                   {:db/id 11}
                   {:db/id 15}]]])
    nil))
