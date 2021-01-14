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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-parented-series [db parent-id-attr parent-id-value parent-child-attr
                           id-attr index-attr]
  (ds/q '[:find ?item-uuid ?i
          :in $
          ?parent-id-attr ?parent-id-value
          ?parent-child-attr ?id-attr ?index-attr
          :where
          [?parent ?parent-id-attr ?parent-id-value]
          [?parent ?parent-child-attr ?child]
          [?child ?id-attr ?item-uuid]
          [?child ?index-attr ?i]]
        db
        parent-id-attr parent-id-value parent-child-attr
        id-attr index-attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn new-item-in-parented-series
  [db [_op parent-lookup-ref parent-child-attr id-attr index-attr tx]]
  (let [[parent-id-attr parent-id-value] parent-lookup-ref
        series (get-parented-series db parent-id-attr parent-id-value parent-child-attr
                                    id-attr index-attr)
        n (if (= 0 (count series))
            0
            (->> (map second series)
                 (reduce max)
                 (inc)))]
    [(assoc tx :i n)
     [:db/add parent-lookup-ref parent-child-attr [id-attr (get tx id-attr)]]]))

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

(defn reorder-item-in-parented-series
  [db [_op parent-lookup-ref parent-child-attr id-attr index-attr id new-index]]
  (let [[parent-id-attr parent-id-value] parent-lookup-ref
        series (->> (get-parented-series db parent-id-attr parent-id-value
                                         parent-child-attr id-attr index-attr)
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

(defn delete-item-in-parented-series
  [db [_op parent-lookup-ref parent-child-attr id-attr index-attr id]]
  (let [[parent-id-attr parent-id-value] parent-lookup-ref
        series (->> (get-parented-series db parent-id-attr parent-id-value
                                         parent-child-attr id-attr index-attr)
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

(defn migrate-item-across-parented-series
  [db [_op src-parent-lookup-ref dst-parent-lookup-ref parent-child-attr
       id-attr index-attr id new-index]]
  (let [[src-id-attr src-id-value] src-parent-lookup-ref
        src-series (->> (get-parented-series db src-id-attr src-id-value parent-child-attr
                                             id-attr index-attr)
                        (sort-by second))
        [dst-id-attr dst-id-value] dst-parent-lookup-ref
        dst-series (->> (get-parented-series db dst-id-attr dst-id-value parent-child-attr
                                             id-attr index-attr)
                        (sort-by second))]
    (if (= 0 (count src-series))
      []
      (let [old-index (some (fn [[id* i]] (if (= id id*) i nil))
                            src-series)]
        (if (not old-index)
          []
          (let [tx-data [[:db/retract
                          src-parent-lookup-ref parent-child-attr [id-attr id]]
                         [:db/add
                          dst-parent-lookup-ref parent-child-attr [id-attr id]]]
                ;; new src series indexes
                tx-data
                (into
                 tx-data
                 (->> (map first src-series)
                      (setval [old-index] NONE)
                      (map-indexed (fn [i id*] [:db/add [id-attr id*] index-attr i]))))

                ;; new dst series indexes
                new-index (min (count dst-series) new-index)
                tx-data
                (into
                 tx-data
                 (->> (map first dst-series)
                      (setval [(srange new-index new-index)] [id])
                      (map-indexed (fn [i id*] [:db/add [id-attr id*] index-attr i]))))]
            ; (prn tx-data)
            tx-data))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eval-tx [db tx]
  (cond
   (map? tx) [tx]
   (vector? tx)
   (let [[op] tx]
     (case op
       :sy/only-if-exists (only-if-exists db tx)

       :sy/new-item-in-series (new-item-in-series db tx)
       :sy/reorder-item-in-series (reorder-item-in-series db tx)
       :sy/delete-item-in-series (delete-item-in-series db tx)

       :sy/new-item-in-parented-series (new-item-in-parented-series db tx)
       :sy/reorder-item-in-parented-series (reorder-item-in-parented-series db tx)
       :sy/delete-item-in-parented-series (delete-item-in-parented-series db tx)
       :sy/migrate-item-across-parented-series (migrate-item-across-parented-series db tx)

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
