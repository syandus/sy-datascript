(ns test.series
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [com.rpl.specter :as sp :refer [setval srange ALL NONE]]
   [datascript.core :as ds]
   [posh.reagent :as posh]
   [clojure.string :as str]
   [sy-datascript.core :as sds :refer [transact!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest reorder-non-existent-item-1
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *db [[:sy/reorder-item-in-series :uuid :i "a" 0]])
    (is (= 0 (count (ds/datoms @*db :eavt))))))

(deftest reorder-non-existent-item-2
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *db [{:uuid "a" :i 0}])
    (is (= 2 (count (ds/datoms @*db :eavt))))
    (transact! *db [[:sy/reorder-item-in-series :uuid :i "b" 1]])
    (is (= 2 (count (ds/datoms @*db :eavt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest reorder-1-item-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0}])
    (transact! *db [[:sy/reorder-item-in-series :uuid :i "a" 0]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    (transact! *db [[:sy/reorder-item-in-series :uuid :i "a" 0]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    (transact! *db [[:sy/reorder-item-in-series :uuid :i "a" 1]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest reorder-2-item-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1}])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))

    (transact! *db [[:sy/reorder-item-in-series :uuid :i "b" 0]])

    (is (= 1 (get-attr :i [:uuid "a"])))
    (is (= 0 (get-attr :i [:uuid "b"])))

    nil))
(deftest reorder-4-item-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "d" :i 2} {:uuid "c" :i 3}])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (is (= 2 (get-attr :i [:uuid "d"])))
    (is (= 3 (get-attr :i [:uuid "c"])))

    (transact! *db [[:sy/reorder-item-in-series :uuid :i "c" 2]])

    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (is (= 2 (get-attr :i [:uuid "c"])))
    (is (= 3 (get-attr :i [:uuid "d"])))

    nil))

(deftest reorder-eof-item-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    ; (print "reorder-eof-item-test")
    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2} {:uuid "d" :i 3}])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (is (= 2 (get-attr :i [:uuid "c"])))
    (is (= 3 (get-attr :i [:uuid "d"])))

    (transact! *db [[:sy/reorder-item-in-series :uuid :i "a" 4]])

    (is (= 0 (get-attr :i [:uuid "b"])))
    (is (= 1 (get-attr :i [:uuid "c"])))
    (is (= 2 (get-attr :i [:uuid "d"])))
    (is (= 3 (get-attr :i [:uuid "a"])))

    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest new-item-0-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [[:sy/new-item-in-series :uuid :i {:uuid "a"}]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest new-item-1-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0}])
    (transact! *db [[:sy/new-item-in-series :uuid :i {:uuid "b"}]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))

    nil))

(deftest new-item-misorder-test
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 1}])
    (transact! *db [[:sy/new-item-in-series :uuid :i {:uuid "b"}]])
    (is (= 1 (get-attr :i [:uuid "a"])))
    (is (= 2 (get-attr :i [:uuid "b"])))

    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest delete-nonexistent-test-1
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *db [[:sy/delete-item-in-series :uuid :i "a" 0]])
    (is (= 0 (count (ds/datoms @*db :eavt))))))


(deftest delete-nonexistent-test-2
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})]
    (transact! *db [{:uuid "a" :i 0}])
    (is (= 2 (count (ds/datoms @*db :eavt))))
    (transact! *db [[:sy/delete-item-in-series :uuid :i "b" 1]])
    (is (= 2 (count (ds/datoms @*db :eavt))))
    nil))

(deftest delete-item-test-1
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0}])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "a"]])
    (is (= 0 (count (ds/datoms @*db :eavt))))
    nil))

(deftest delete-item-test-2
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1}])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "a"]])
    (is (= 0 (get-attr :i [:uuid "b"])))
    (transact! *db [[:sy/delete-item-in-series :uuid :i "b"]])
    (is (= 0 (count (ds/datoms @*db :eavt))))

    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1}])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "b"]])
    (is (= 0 (get-attr :i [:uuid "a"])))

    nil))

(deftest delete-item-test-3
  (let [*db (ds/create-conn {:uuid {:db/unique :db.unique/identity}})
        get-attr (fn [k e]
                   (-> (ds/pull @*db [k] e)
                       k))]
    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2}])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "a"]])
    (is (= 0 (get-attr :i [:uuid "b"])))
    (is (= 1 (get-attr :i [:uuid "c"])))
    (transact! *db [[:sy/delete-item-in-series :uuid :i "b"]])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "c"]])

    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2}])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "b"]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "c"])))
    (transact! *db [[:sy/delete-item-in-series :uuid :i "a"]])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "c"]])

    (transact! *db [{:uuid "a" :i 0} {:uuid "b" :i 1} {:uuid "c" :i 2}])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "c"]])
    (is (= 0 (get-attr :i [:uuid "a"])))
    (is (= 1 (get-attr :i [:uuid "b"])))
    (transact! *db [[:sy/delete-item-in-series :uuid :i "a"]])
    (transact! *db [[:sy/delete-item-in-series :uuid :i "b"]])

    nil))
