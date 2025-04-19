(ns vary.core
  (:require [malli.core :as m]
            [malli.error :as me]))

(defonce variants (atom {}))

(defn generate-schema
  "Generates a Malli schema from a case map defining variant cases."
  [case-map]
  (let [schemas (map (fn [[case-kw args-or-props]]
                       (if (map? args-or-props)
                         [:= case-kw]
                         (if (empty? args-or-props)
                           [:= case-kw]
                           [:map
                            [:type [:= case-kw]]
                            (vec (mapcat (fn [arg type]
                                           [(keyword arg) type])
                                         (take-nth 2 args-or-props)
                                         (take-nth 2 (rest args-or-props))))])))
                     case-map)]
    (into [:or] schemas)))

(defn generate-accessors
  "Generates accessor functions for all unique properties in the case map."
  [vary-name case-map]
  (let [all-keys (->> (vals case-map)
                      (mapcat (fn [v]
                                (cond
                                  (map? v)    (keys v)
                                  (vector? v) (->> (partition 2 v) (map first))
                                  :else nil)))
                      set)]
    (for [k all-keys]
      (let [fname (symbol (str vary-name "-" (name k)))
            kw    (keyword k)]
        `(defn ~fname [case#]
           (get-in (cases ~vary-name) [case# ~kw]))))))

(defmacro vary
  "Defines a variant with a Malli schema, case metadata, and accessor functions.
   Usage:
   (vary CoffeeSize
     {:small  {:label \"Small\" :price 3.50 :volume 8}
      :medium {:label \"Medium\" :price 4.00 :volume 12}
      :large  {:label \"Large\" :price 4.50 :volume 16}})
   (vary OrderStatus
     {:pending []
      :completed [:amount :double]
      :failed [:reason :string]})"
  [vary-name case-map]
  (let [schema    (generate-schema case-map)
        accessors (generate-accessors vary-name case-map)
        metadata  (zipmap (keys case-map) (vals case-map))]
    `(do
       (swap! variants assoc '~vary-name
              {:schema (m/schema ~schema)
               :cases '~metadata})
       (def ~vary-name (m/schema ~schema))
       ~@accessors)))

(defmacro cases
  "Returns the cases of a variant as a map.
   Usage: (cases CoffeeSize)"
  [vary-name]
  `(get-in @variants ['~vary-name :cases]))

(defmacro match
  "Matches a value against variant cases with validation.
   Usage:
     (match size CoffeeSize
       :small  3.50
       :medium 4.00
       :large  4.50)
     (match order OrderStatus
       :pending               \"Order is pending\"
       [:completed amount]    (str \"Completed with amount: \" amount)
       [:failed reason]       (str \"Failed: \" reason))"
  [value vary-name & clauses]
  (let [cases-var (get-in @variants [vary-name :cases])
        get-meta  (fn [kw] (get cases-var kw))
        pairs     (partition 2 clauses)
        val-sym   (gensym "val")]
    `(let [~val-sym ~value]
       (if (m/validate ~vary-name ~val-sym)
         (cond
           ~@(mapcat
              (fn [[pattern body]]
                (if (vector? pattern)
                  (let [case-kw   (first pattern)
                        arg-names (rest pattern)
                        meta-v    (get-meta case-kw)
                        meta-map  (when (vector? meta-v)
                                    (apply array-map meta-v))
                        bindings  (mapcat
                                   (fn [arg]
                                     `[~arg (get ~val-sym ~(keyword arg))])
                                   arg-names)]
                    [`(= (get ~val-sym :type) ~case-kw)
                     `(let [~@bindings]
                        ~body)])
                  [`(= ~val-sym ~pattern)
                   body]))
              pairs)
           :else
           (throw (ex-info "No matching case" {:value ~val-sym})))
         (throw (ex-info "Invalid variant"
                         {:value ~val-sym
                          :error (m/explain ~vary-name ~val-sym)}))))))



(comment

  (vary Fruit {:apple  {:color "red"}
               :banana {:color "yellow"}})

  (vary CoffeeSize
        {:small  {:label "Small" :price 3.50 :volume 8}
         :medium {:label "Medium" :price 4.00 :volume 12}
         :large  {:label "Large" :price 4.50 :volume 16}})

  (vary OrderStatus
        {:pending   []
         :completed [:amount :double]
         :failed    [:reason :string]})

  (cases Fruit)
  (cases CoffeeSize)
  (cases OrderStatus)


  (CoffeeSize-label :small)
  (CoffeeSize-price :small)
  (CoffeeSize-volume :small)

  (Fruit-color :apple)
  (Fruit-color :banana)



  ;;
  )
