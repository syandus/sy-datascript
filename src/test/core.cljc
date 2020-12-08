(ns test.core
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [datascript.core :as ds]
   [sy-datascript.core :as sds :refer [transact!]]
   [test.series]
   [test.parented-series]))


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

