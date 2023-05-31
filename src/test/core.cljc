(ns test.core
  (:require
   [clojure.test :as t :refer [is deftest]]
   [datascript.core :as ds]
   [sy-datascript.core :as sds :refer [transact!]]
   [test.series]
   [test.parented-series]))


(deftest passthrough-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                     (-> (ds/pull @*db [k] e)
                         k))]
    (transact! *db [{:uuid 1 :foo :bar}])
    (is (= :bar (get-attr :foo [:uuid 1])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest only-if-exists-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                     (-> (ds/pull @*db [k] e)
                         k))]
    (transact! *db [{:uuid 1 :x 1}])
    (let [id 1]
      (transact! *db [[:sy/only-if-exists :uuid id
                         [:db/add [:uuid id] :y 2]]])
      (is (= 1 (count (ds/datoms @*db :avet :uuid id))))
      (is (= 2 (get-attr :y [:uuid id]))))
    (let [id 2]
      (transact! *db [[:sy/only-if-exists :uuid id
                       [:db/add [:uuid id] :y 2]]])
      (is (= 0 (count (ds/datoms @*db :avet :uuid id)))))
    
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest add-time-in-seconds-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [[:sy/add-time-in-seconds :t
                     {:uuid "abc" :text "Hello, World!"}]])
    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))
    (is (number? (get-attr :t [:uuid "abc"])))
    nil))

(deftest compare-and-swap-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))
        e [:uuid "foo"]]
    (transact! *db [{:uuid "foo" :x "foo"}])

    (is (= "foo" (get-attr :x e)))
    (is (= nil (get-attr :cas e)))

    (transact! *db [[:sy/compare-and-swap e :cas 0
                     [:db/add e :x "bar"]]])
    (is (= "bar" (get-attr :x e)))
    (is (= 1 (get-attr :cas e)))

    ;; ignored tx
    (transact! *db [[:sy/compare-and-swap e :cas 0
                     [:db/add e :x "fizz"]]])
    (is (= "bar" (get-attr :x e)))
    (is (= 1 (get-attr :cas e)))

    (transact! *db [[:sy/compare-and-swap e :cas 1
                     [:db/add e :x "buzz"]]])
    (is (= "buzz" (get-attr :x e)))
    (is (= 2 (get-attr :cas e)))
    
    (transact! *db [[:sy/compare-and-swap e :cas 2
                     [:db/add e :cat "cat"]
                     [:db/add e :dog "dog"]]])
    (is (= 3 (get-attr :cas e)))
    (is (= "cat" (get-attr :cat e)))
    (is (= "dog" (get-attr :dog e)))

    nil))

(deftest cas-recursive-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))
        e [:uuid "foo"]]
    (transact! *db [{:uuid "foo" :x "foo"}])
    (is (= "foo" (get-attr :x e)))

    (transact! *db [[:sy/compare-and-swap [:uuid "foo"] :cas 0
                     [:sy/add-time-in-seconds :t
                      {:uuid "bar" :text "Hello, World!"}]]])

    (is (= 1 (get-attr :cas [:uuid "foo"])))
    (is (= "Hello, World!" (get-attr :text [:uuid "bar"])))
    (is (number? (get-attr :t [:uuid "bar"])))

    (transact! *db [[:sy/compare-and-swap [:uuid "foo"] :cas 0
                     [:sy/add-time-in-seconds :t
                      {:uuid "bar" :text "NOP"}]]])
    (is (= "Hello, World!" (get-attr :text [:uuid "bar"])))

    (transact! *db [[:sy/compare-and-swap [:uuid "foo"] :cas 1
                     [:db/add [:uuid "bar"] :y 42]]])
    (is (= 42 (get-attr :y [:uuid "bar"])))

    ; (doall (map #(prn %) (ds/datoms @*db :eavt)))

    nil))

(deftest duplication-prevention-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))
        ;; dump! (fn [] (doall (map #(prn %) (ds/datoms @*db :eavt))))
        ]
    (transact! *db [[:sy/new-item-in-series :uuid :i {:uuid "foo" :x "foo"}]])
    (is (= 3 (count (ds/datoms @*db :eavt))))
    (is (= "foo" (get-attr :x [:uuid "foo"])))

    (transact! *db [[:sy/new-item-in-series :uuid :i {:uuid "foo" :x "bar"}]])
    ;; (dump!)
    (is (= 3 (count (ds/datoms @*db :eavt))))
    (is (= "foo" (get-attr :x [:uuid "foo"])))))

(deftest parented-duplication-prevention-test
  (let [*db (ds/create-conn {:parent/uuid {:db/unique :db.unique/identity}
                             :child/uuid {:db/unique :db.unique/identity}
                             :parent/child {:db/valueType :db.type/ref
                                            :db/cardinality :db.cardinality/many
                                            :db/index true}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))
        ;; dump! (fn [] (doall (map #(prn %) (ds/datoms @*db :eavt))))
        ]
    (transact! *db [[:sy/new-item-in-series :uuid :i {:parent/uuid "parent0" :x "foo"}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "parent0"]  :parent/child :child/uuid :i
                     {:child/uuid "child0" :y "bar"}]])
    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "parent0"]  :parent/child :child/uuid :i
                     {:child/uuid "child1" :y "quux"}]])

    (is (= "foo" (get-attr :x [:parent/uuid "parent0"])))
    (is (= 0 (get-attr :i [:parent/uuid "parent0"])))

    (is (= "bar" (get-attr :y [:child/uuid "child0"])))
    (is (= 0 (get-attr :i [:child/uuid "child0"])))

    (is (= "quux" (get-attr :y [:child/uuid "child1"])))
    (is (= 1 (get-attr :i [:child/uuid "child1"])))

    (transact! *db [[:sy/new-item-in-parented-series
                     [:parent/uuid "parent0"]  :parent/child :child/uuid :i
                     {:child/uuid "child1" :y "apple"}]])

    (is (= "quux" (get-attr :y [:child/uuid "child1"])))
    (is (= 1 (get-attr :i [:child/uuid "child1"])))

    nil))
