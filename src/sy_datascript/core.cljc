(ns sy-datascript.core
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [com.rpl.specter :as sp :refer [setval srange ALL NONE]]
   [datascript.core :as ds]
   [posh.reagent :as posh]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn only-if-exists
  [db [_op attr id tx]]
  (let [matches (ds/datoms db :avet attr id)]
    (if (< 0 (count matches))
      [tx]
      [])))

(defn reorder-item-in-series [db [_op id-attr index-attr id new-index]]
  (let [series (->> (ds/q '[:find ?item-uuid ?i
                            :in $ ?id-attr ?index-attr
                            :where
                            [?e ?id-attr ?item-uuid]
                            [?e ?index-attr ?i]]
                          db id-attr index-attr)
                    (sort-by second))]
    (if (or (= 0 (count series)))
      []
      (let [new-index (min (dec (count series)) new-index)
            old-index (some (fn [[id* i]] (if (= id id*) i nil))
                            series)]
        (if (not old-index)
          []
          (->> (map first series)
               (setval [old-index] NONE)
               (setval [(srange new-index new-index)] [id])
               (map-indexed (fn [i id*] [:db/add [id-attr id*] index-attr i]))))))))

(defn new-item-in-series [db [_op id-attr index-attr tx]]
  (let [series (ds/q '[:find ?item-uuid ?i
                       :in $ ?id-attr ?index-attr
                       :where
                       [?e ?id-attr ?item-uuid]
                       [?e ?index-attr ?i]]
                     db id-attr index-attr)
        n (if (= 0 (count series))
            0
            (->> (map second series)
                 (reduce max)
                 (inc)))]
    [(assoc tx :i n)]))

(defn delete-item-in-series [db [_op id-attr index-attr id]]
  (let [series (->> (ds/q '[:find ?item-uuid ?i
                            :in $ ?id-attr ?index-attr
                            :where
                            [?e ?id-attr ?item-uuid]
                            [?e ?index-attr ?i]]
                          db id-attr index-attr)
                    (sort-by second))]
    (if (or (= 0 (count series)))
      []
      (let [old-index (some (fn [[id* i]] (if (= id id*) i nil))
                            series)]
        (if (not old-index)
          []
          (->> (map first series)
               (setval [old-index] NONE)
               (map-indexed (fn [i id*] [:db/add [id-attr id*] index-attr i]))
               (into [[:db.fn/retractEntity [id-attr id]]])))))))

(defn eval-tx [db tx]
  (cond
   (map? tx) [tx]
   (vector? tx)
   (let [[op] tx]
     (case op
       :sy/only-if-exists (only-if-exists db tx)
       :sy/reorder-item-in-series (reorder-item-in-series db tx)
       :sy/new-item-in-series (new-item-in-series db tx)
       :sy/delete-item-in-series (delete-item-in-series db tx)
       [tx]))
   :else [tx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn with
  "Similar to datascript `with`, but supports Syandus extensions to the
  language."
  ([db tx-data] (with db tx-data nil))
  ([db tx-data tx-meta]
   (let [tx-data (->> (mapv (partial eval-tx db) tx-data)
                      (mapcat identity)
                      (vec))]
     (ds/with db tx-data tx-meta))))

(defn sy-transact!
  "Similar to datascript or posh `transact!`, but supports Syandus extensions
  to the language."
  ([transact-fn *conn tx-data] (sy-transact! transact-fn *conn tx-data nil))
  ([transact-fn *conn tx-data tx-meta]
   (assert (vector? tx-data) "`tx-data` must be a vector!")
   (let [db @*conn
         tx-data (->> (mapv (partial eval-tx db) tx-data)
                      (mapcat identity)
                      (vec))]
     (transact-fn *conn tx-data tx-meta))))

(def transact! "[conn tx-data tx-meta]"
  (partial sy-transact! ds/transact!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest passthrough-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                     (-> (ds/pull @*conn [k] e)
                         k))]
    (transact! *conn [{:uuid 1 :foo :bar}])
    (is (= :bar (get-attr :foo [:uuid 1])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest only-if-exists-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                     (-> (ds/pull @*conn [k] e)
                         k))]
    (transact! *conn [{:uuid 1 :x 1}])
    (let [id 1]
      (transact! *conn [[:sy/only-if-exists :uuid id
                         [:db/add [:uuid id] :y 2]]])
      (is (= 1 (count (ds/datoms @*conn :avet :uuid id))))
      (is (= 2 (get-attr :y [:uuid id]))))
    (let [id 2]
      (transact! *conn [[:sy/only-if-exists :uuid id
                       [:db/add [:uuid id] :y 2]]])
      (is (= 0 (count (ds/datoms @*conn :avet :uuid id)))))
    
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest reorder-non-existent-item-1
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "a" 0]])
    (is (= 0 (count (ds/datoms @*conn :eavt))))))

(deftest reorder-non-existent-item-2
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *conn [{:uuid "a" :i 0}])
    (is (= 2 (count (ds/datoms @*conn :eavt))))
    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "b" 1]])
    (is (= 2 (count (ds/datoms @*conn :eavt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest reorder-1-item-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0}])
    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "a" 0]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "a" 0]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "a" 1]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest reorder-2-item-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1}])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))

    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "b" 0]])

    (is (= 1 (get-attr :i [:uuid "a"])))
    (is (= 0 (get-attr :i [:uuid "b"])))

    nil))
(deftest reorder-4-item-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "d" :i 2} {:uuid "c" :i 3}])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (is (= 2 (get-attr :i [:uuid "d"])))
    (is (= 3 (get-attr :i [:uuid "c"])))

    (transact! *conn [[:sy/reorder-item-in-series :uuid :i "c" 2]])

    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (is (= 2 (get-attr :i [:uuid "c"])))
    (is (= 3 (get-attr :i [:uuid "d"])))

    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest new-item-0-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [[:sy/new-item-in-series :uuid :i {:uuid "a"}]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest new-item-1-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0}])
    (transact! *conn [[:sy/new-item-in-series :uuid :i {:uuid "b"}]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))

    nil))

(deftest new-item-misorder-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 1}])
    (transact! *conn [[:sy/new-item-in-series :uuid :i {:uuid "b"}]])
    (is (= 1 (get-attr :i [:uuid "a"])))
    (is (= 2 (get-attr :i [:uuid "b"])))

    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest delete-nonexistent-test-1
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "a" 0]])
    (is (= 0 (count (ds/datoms @*conn :eavt))))))


(deftest delete-nonexistent-test-2
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *conn [{:uuid "a" :i 0}])
    (is (= 2 (count (ds/datoms @*conn :eavt))))
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "b" 1]])
    (is (= 2 (count (ds/datoms @*conn :eavt))))
    nil))

(deftest delete-item-test-1
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0}])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "a"]])
    (is (= 0 (count (ds/datoms @*conn :eavt))))
    nil))

(deftest delete-item-test-2
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1}])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "a"]])
    (is (= 0 (get-attr :i [:uuid "b"])))
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "b"]])
    (is (= 0 (count (ds/datoms @*conn :eavt))))

    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1}])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "b"]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest delete-item-test-3
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2}])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "a"]])
    (is (= 0 (get-attr :i [:uuid "b"])))
    (is (= 1 (get-attr :i [:uuid "c"])))
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "b"]])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "c"]])

    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2}])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "b"]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "c"])))
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "a"]])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "c"]])

    (transact! *conn [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2}])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "c"]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "a"]])
    (transact! *conn [[:sy/delete-item-in-series :uuid :i "b"]])

    nil))
