(ns vary.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [vary.core :refer [vary cases match]]
            [malli.core :as m]))

(deftest test-vary
  (testing "Schema validation"
    (vary CoffeeSize
          {:small  {:label "Small" :price 3.50 :volume 8}
           :medium {:label "Medium" :price 4.00 :volume 12}
           :large  {:label "Large" :price 4.50 :volume 16}})
    (is (m/validate CoffeeSize :small) "Small size should be valid")
    (is (m/validate CoffeeSize :medium) "Medium size should be valid")
    (is (m/validate CoffeeSize :large) "Large size should be valid")
    (is (not (m/validate CoffeeSize :invalid)) "Invalid size should fail validation"))

  (testing "Metadata storage"
    (let [variant-data (get @vary.core/variants 'CoffeeSize)
          expected-cases {:small  {:label "Small" :price 3.50 :volume 8}
                         :medium {:label "Medium" :price 4.00 :volume 12}
                         :large  {:label "Large" :price 4.50 :volume 16}}]
      (is (= (:cases variant-data) expected-cases)
          "CoffeeSize cases should match the defined cases")
      (is (= (m/form (:schema variant-data))
             [:or [:= :small] [:= :medium] [:= :large]])
          "CoffeeSize schema should match the expected form")))

  (testing "Accessors"
    (is (= "Small" (CoffeeSize-label :small)) "Label accessor should return Small")
    (is (= 3.50 (CoffeeSize-price :small)) "Price accessor should return 3.50")
    (is (= 8 (CoffeeSize-volume :small)) "Volume accessor should return 8")
    (is (= nil (CoffeeSize-price :invalid)) "Accessor for invalid case should return nil")))

(deftest test-cases
  (vary CoffeeSize
        {:small  {:label "Small"}
         :medium {:label "Medium"}})
  (is (= (cases CoffeeSize)
         {:small  {:label "Small"}
          :medium {:label "Medium"}})
      "Cases should return the defined case map"))

(deftest test-match
  (vary CoffeeSize
        {:small  {}
         :medium {}
         :large  {}})
  (vary OrderStatus
        {:pending   []
         :completed [:amount :double]
         :failed    [:reason :string]})

  (testing "Simple cases"
    (is (= 3.50 (match :small CoffeeSize
                  :small  3.50
                  :medium 4.00
                  :large  4.50))
        "Matching :small should return 3.50")
    (is (= 4.00 (match :medium CoffeeSize
                  :small  3.50
                  :medium 4.00
                  :large  4.50))
        "Matching :medium should return 4.00"))

  (testing "Associated values"
    (is (= "Completed with amount: 5.5"
           (match {:type :completed :amount 5.5} OrderStatus
             :pending            "Pending"
             [:completed amount] (str "Completed with amount: " amount)
             [:failed reason]    "Failed"))
        "Matching completed order should use default double representation")
    (is (= "Failed: Out of stock"
           (match {:type :failed :reason "Out of stock"} OrderStatus
             :pending            "Pending"
             [:completed amount] "Completed"
             [:failed reason]    (str "Failed: " reason)))
        "Matching failed order should return reason"))

  (testing "Validation"
    (is (thrown? Exception
                 (match :invalid CoffeeSize
                   :small  3.50
                   :medium 4.00
                   :large  4.50))
        "Invalid CoffeeSize should throw an exception")
    (is (thrown? Exception
                 (match {:type :completed :amount "not a number"} OrderStatus
                   :pending            "Pending"
                   [:completed amount] amount
                   [:failed reason]    reason))
        "Invalid amount type should throw an exception"))

  (testing "Unmatched cases"
    (is (thrown? Exception
                 (match :large CoffeeSize
                   :small  3.50
                   :medium 4.00))
        "Unmatched case should throw an exception")))

(deftest test-generate-schema
  (testing "Simple cases"
    (is (= (vary.core/generate-schema {:small {} :medium {}})
           [:or [:= :small] [:= :medium]])
        "Simple cases should generate an OR schema"))
  (testing "Cases with associated values"
    (is (= (vary.core/generate-schema {:completed [:amount :double]})
           [:or [:map [:type [:= :completed]] [:amount :double]]])
        "Cases with args should generate a map schema")))

(deftest test-generate-accessors
  (testing "Accessor generation"
    (let [accessors (vary.core/generate-accessors 'CoffeeSize
                                                  {:small  {:price 3.50}
                                                   :medium {:price 4.00}})]
      (is (= (count accessors) 1) "Should generate one accessor for :price")
      (is (some #(= 'CoffeeSize-price (second %)) accessors)
          "Should include CoffeeSize-price function"))))
