(ns test.parented-series
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [com.rpl.specter :as sp :refer [setval srange ALL NONE]]
   [datascript.core :as ds]
   [posh.reagent :as posh]
   [clojure.string :as str]
   [sy-datascript.core :as sds :refer [transact!]]))

(deftest new-item-in-parented-series-test
  (let [*conn (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                               :parent/child {:db/valueType :db.type/ref
                                              :db/cardinality :db.cardinality/many}
                               :child/uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:parent/uuid "p1"}
                      {:parent/uuid "p2"}])
    (is (= 1 (get-attr :db/id [:parent/uuid "p1"])))
    (is (= 2 (get-attr :db/id [:parent/uuid "p2"])))
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i
                       {:child/uuid "c1" :child/data :foo}]])
    (is (= 3 (get-attr :db/id [:child/uuid "c1"])))
    (is (= 0 (get-attr :i [:child/uuid "c1"])))

    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i
                       {:child/uuid "c2" :child/data :foo}]])
    (is (= 4 (get-attr :db/id [:child/uuid "c2"])))
    (is (= 1 (get-attr :i [:child/uuid "c2"])))

    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i
                       {:child/uuid "ch1" :child/data :foo}]])
    (is (= 5 (get-attr :db/id [:child/uuid "ch1"])))
    (is (= 0 (get-attr :i [:child/uuid "ch1"])))
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i
                       {:child/uuid "ch2" :child/data :foo}]])
    (is (= 6 (get-attr :db/id [:child/uuid "ch2"])))
    (is (= 1 (get-attr :i [:child/uuid "ch2"])))

    ; (doall (map #(prn %) (ds/datoms @*conn :eavt)))
    nil))

(deftest reorder-item-in-parented-series-test
  (let [*conn (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                               :parent/child {:db/valueType :db.type/ref
                                              :db/cardinality :db.cardinality/many}
                               :child/uuid {:db/unique :db.unique/identity}})
        check (fn [[lr a v]]
                (is (= v (get (ds/pull @*conn [a] lr) a))))
        check-all (fn [tests]
                    (doall (map check tests)))]
    (transact! *conn [{:parent/uuid "p1"}
                      {:parent/uuid "p2"}])
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i
                       {:child/uuid "c1" :child/data :foo}]])
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i
                       {:child/uuid "c2" :child/data :foo}]])

    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i
                       {:child/uuid "ch1" :child/data :foo}]])
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i
                       {:child/uuid "ch2" :child/data :foo}]])
    (check-all [[[:parent/uuid "p1"] :db/id 1]
                [[:parent/uuid "p2"] :db/id 2]
                [[:child/uuid "c1"] :i 0]
                [[:child/uuid "c2"] :i 1]
                [[:child/uuid "ch1"] :i 0]
                [[:child/uuid "ch2"] :i 1]])

    (transact! *conn [[:sy/reorder-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i "c1" 1]])
    (check-all [[[:child/uuid "c1"] :i 1]
                [[:child/uuid "c2"] :i 0]
                [[:child/uuid "ch1"] :i 0]
                [[:child/uuid "ch2"] :i 1]])

    (transact! *conn [[:sy/reorder-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i "ch1" 1]])
    (check-all [[[:child/uuid "c1"] :i 1]
                [[:child/uuid "c2"] :i 0]
                [[:child/uuid "ch1"] :i 1]
                [[:child/uuid "ch2"] :i 0]])

    (transact! *conn [[:sy/reorder-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i "c1" 0]
                      [:sy/reorder-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i "ch1" 0]])
    (check-all [[[:parent/uuid "p1"] :db/id 1]
                [[:parent/uuid "p2"] :db/id 2]
                [[:child/uuid "c1"] :i 0]
                [[:child/uuid "c2"] :i 1]
                [[:child/uuid "ch1"] :i 0]
                [[:child/uuid "ch2"] :i 1]])

    ; (doall (map #(prn %) (ds/datoms @*conn :eavt)))
    nil))

(deftest delete-item-in-parented-series-test
  (let [*conn (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                               :parent/child {:db/valueType :db.type/ref
                                              :db/cardinality :db.cardinality/many}
                               :child/uuid {:db/unique :db.unique/identity}})
        check (fn [[lr a v]]
                (is (= v (get (ds/pull @*conn [a] lr) a))))
        check-all (fn [tests]
                    (doall (map check tests)))]

    (transact! *conn [{:parent/uuid "p1"}
                      {:parent/uuid "p2"}])
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i
                       {:child/uuid "c1" :child/data :foo}]])
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i
                       {:child/uuid "c2" :child/data :foo}]])

    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i
                       {:child/uuid "ch1" :child/data :foo}]])
    (transact! *conn [[:sy/new-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i
                       {:child/uuid "ch2" :child/data :foo}]])

    (check-all [[[:parent/uuid "p1"] :db/id 1]
                [[:parent/uuid "p2"] :db/id 2]
                [[:child/uuid "c1"] :i 0]
                [[:child/uuid "c2"] :i 1]
                [[:child/uuid "ch1"] :i 0]
                [[:child/uuid "ch2"] :i 1]])

    (transact! *conn [[:sy/delete-item-in-parented-series
                       [:parent/uuid "p1"] :parent/child
                       :child/uuid :i "c1"]])
    (check-all [[[:parent/uuid "p1"] :db/id 1]
                [[:parent/uuid "p2"] :db/id 2]
                [[:child/uuid "c2"] :i 0]
                [[:child/uuid "ch1"] :i 0]
                [[:child/uuid "ch2"] :i 1]])

    (transact! *conn [[:sy/delete-item-in-parented-series
                       [:parent/uuid "p2"] :parent/child
                       :child/uuid :i "ch2"]])
    (check-all [[[:parent/uuid "p1"] :db/id 1]
                [[:parent/uuid "p2"] :db/id 2]
                [[:child/uuid "c2"] :i 0]
                [[:child/uuid "ch1"] :i 0]])

    (doall (map #(prn %) (ds/datoms @*conn :eavt)))
    nil))
