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

(defn reorder-item [db [_op id-attr index-attr id new-index]]
  (let [series (->> (ds/q '[:find ?item-uuid ?i
                            :in $ ?id-attr ?index-attr
                            :where
                            [?e ?id-attr ?item-uuid]
                            [?e ?index-attr ?i]]
                          db id-attr index-attr)
                    (sort-by second))
        new-index (min (dec (count series)) new-index)
        old-index (some (fn [[id* i]] (if (= id id*) i nil)) series)
        tx-data (->> (map first series)
                     (setval [old-index] NONE)
                     (setval [(srange new-index new-index)] id)
                     (map-indexed (fn [i id*] [:db/add [id-attr id*] index-attr i])))]
    tx-data))

(defn new-item [db [_op id-attr index-attr tx]]
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

(defn eval-tx [db tx]
  (cond
   (map? tx) [tx]
   (vector? tx)
   (let [[op] tx]
     (case op
       :sy/only-if-exists (only-if-exists db tx)
       :sy/reorder-item (reorder-item db tx)
       :sy/new-item (new-item db tx)
       [tx]))
   :else [tx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sy-transact!
  "Similar to datascript or posh `transact!`, but support Syandus extensions to
  the language."
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


(deftest reorder-1-item-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0}])
    (transact! *conn [[:sy/reorder-item :uuid :i "a" 0]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    (transact! *conn [[:sy/reorder-item :uuid :i "a" 0]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    (transact! *conn [[:sy/reorder-item :uuid :i "a" 1]])
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

    (transact! *conn [[:sy/reorder-item :uuid :i "b" 0]])

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

    (transact! *conn [[:sy/reorder-item :uuid :i "c" 2]])

    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (is (= 2 (get-attr :i [:uuid "c"])))
    (is (= 3 (get-attr :i [:uuid "d"])))

    nil))

(deftest new-item-0-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [[:sy/new-item :uuid :i {:uuid "a"}]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest new-item-1-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 0}])
    (transact! *conn [[:sy/new-item :uuid :i {:uuid "b"}]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))

    nil))

(deftest new-item-misorder-test
  (let [*conn (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*conn [k] e)
                       k))]
    (transact! *conn [{:uuid "a" :i 1}])
    (transact! *conn [[:sy/new-item :uuid :i {:uuid "b"}]])
    (is (= 1 (get-attr :i [:uuid "a"])))
    (is (= 2 (get-attr :i [:uuid "b"])))

    nil))
