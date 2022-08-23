(ns sy-datascript.core
  (:require
   [clojure.math :refer [floor]]
   [com.rpl.specter :as sp :refer [setval srange before-index ALL NONE]]
   [datascript.core :as ds]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn only-if-exists
  [db [_op attr id tx]]
  (let [matches (ds/datoms db :avet attr id)]
    (if (< 0 (count matches))
      [tx]
      [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-parented-series
  "Returns a seq of [\"UUID string\" index-number]"
  [db parent-id-attr parent-id-value parent-child-attr
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
    [(assoc tx index-attr n)]))

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
    [(assoc tx index-attr n)
     [:db/add parent-lookup-ref parent-child-attr [id-attr (get tx id-attr)]]]))

(defn reorder-item-in-series [db [_op id-attr index-attr id new-index]]
  (let [series (->> (ds/q '[:find ?item-uuid ?i
                            :in $ ?id-attr ?index-attr
                            :where
                            [?e ?id-attr ?item-uuid]
                            [?e ?index-attr ?i]]
                          db id-attr index-attr)
                    (sort-by second))]
    (if (= 0 (count series))
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
    (if (= 0 (count series))
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
    (if (= 0 (count series))
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
    (if (= 0 (count series))
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

(defn insert-item-in-parented-series
  [db [_op
       parent-lookup-ref parent-child-attr
       child-id-attr index-attr
       new-child-id idx]]
  (let [[parent-id-attr parent-id-value] parent-lookup-ref
        series (->> (get-parented-series db parent-id-attr parent-id-value
                                         parent-child-attr child-id-attr index-attr)
                    (sort-by second))
        curr-n (count series)
        idx (max 0 idx)
        idx (min curr-n idx)]
    (->> (map first series)
         (setval [(before-index idx)] new-child-id)
         (map-indexed (fn [i child-id] [:db/add [child-id-attr child-id] index-attr i])))))

(defn remove-item-in-parented-series
  [db [_op parent-lookup-ref parent-child-attr id-attr index-attr id]]
  (let [[parent-id-attr parent-id-value] parent-lookup-ref
        series (->> (get-parented-series db parent-id-attr parent-id-value
                                         parent-child-attr id-attr index-attr)
                    (sort-by second))]
    (if (= 0 (count series))
      []
      (let [old-index (some (fn [[id* i]] (if (= id id*) i nil))
                            series)
            child-ref [id-attr id]]
        (if (not old-index)
          []
          (->> (map first series)
               (setval [old-index] NONE)
               (map-indexed (fn [i id*] [:db/add [id-attr id*] index-attr i]))
               (into [[:db.fn/retractAttribute child-ref index-attr]
                      [:db/retract parent-lookup-ref parent-child-attr child-ref]])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *unix-timestamp* nil)

(defn add-time-in-seconds [_db [_op attr tx]]
  (let [t (if-not (nil? *unix-timestamp*)
            *unix-timestamp*
            (-> #?(:cljs (js/Date.now) :clj (System/currentTimeMillis))
                (/ 1000)
                (floor)))]
    [(assoc tx attr t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compare-and-swap [db [_op
                            entity-ref cas-attribute value
                            & txs]]
  (let [x (-> (ds/pull db [cas-attribute] entity-ref)
              (get cas-attribute)
              (or 0))]
    (if (= x value)
      (into [[:db/add entity-ref cas-attribute (inc x)]]
            txs)
      [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eval-tx [db tx]
  (cond
    (map? tx) [tx]
    (vector? tx)
    (let [[op] tx]
      (case op
        :sy/only-if-exists (only-if-exists db tx)
        :sy/compare-and-swap (compare-and-swap db tx)

        :sy/new-item-in-series (new-item-in-series db tx)
        :sy/reorder-item-in-series (reorder-item-in-series db tx)
        :sy/delete-item-in-series (delete-item-in-series db tx)

        :sy/new-item-in-parented-series (new-item-in-parented-series db tx)
        :sy/reorder-item-in-parented-series (reorder-item-in-parented-series db tx)
        :sy/delete-item-in-parented-series (delete-item-in-parented-series db tx)
        :sy/migrate-item-across-parented-series (migrate-item-across-parented-series db tx)

        :sy/insert-item-in-parented-series (insert-item-in-parented-series db tx)
        :sy/remove-item-in-parented-series (remove-item-in-parented-series db tx)

        :sy/add-time-in-seconds (add-time-in-seconds db tx)

        [tx]))
    :else [tx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ops #{:sy/only-if-exists
           :sy/compare-and-swap

           :sy/new-item-in-series
           :sy/reorder-item-in-series
           :sy/delete-item-in-series

           :sy/new-item-in-parented-series
           :sy/reorder-item-in-parented-series
           :sy/delete-item-in-parented-series
           :sy/migrate-item-across-parented-series

           :sy/insert-item-in-parented-series
           :sy/remove-item-in-parented-series

           :sy/add-time-in-seconds})

(defn sy-op? [tx]
  (cond
    (map? tx) false
    (vector? tx) (let [[op] tx]
                   (contains? ops op))))

(defn eval-ops-loop [db input-tx-data]
  (-> (loop [tx-data input-tx-data]
        (if (some sy-op? tx-data)
          (recur (->> (map (partial eval-tx db) tx-data)
                      (mapcat identity)))
          tx-data))
      (vec)))

(defn with
  "Similar to datascript `with`, but supports Syandus extensions to the
  language."
  ([db tx-data] (with db tx-data nil))
  ([db tx-data tx-meta]
   (let [tx-data (eval-ops-loop db tx-data)]
     (ds/with db tx-data tx-meta))))

(defn sy-transact!
  "Similar to datascript or posh `transact!`, but supports Syandus extensions
  to the language."
  ([transact-fn *conn tx-data] (sy-transact! transact-fn *conn tx-data nil))
  ([transact-fn *conn tx-data tx-meta]
   (assert (vector? tx-data) "`tx-data` must be a vector!")
   (let [db @*conn
         tx-data (eval-ops-loop db tx-data)]
     (transact-fn *conn tx-data tx-meta))))

(def transact! "[conn tx-data tx-meta]"
  (partial sy-transact! ds/transact!))
