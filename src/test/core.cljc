(ns test.core
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
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
    
    
    nil))
