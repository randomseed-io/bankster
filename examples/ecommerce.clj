(ns io.randomseed.bankster.examples.ecommerce
  "E-commerce operations: shopping cart, VAT, commissions, discounts.

   Demonstrates precise financial arithmetic without the rounding
   errors typical of float/double."
  (:require [io.randomseed.bankster.money :as m]
            [io.randomseed.bankster.scale :as scale]))

;;; ---------------------------------------------------------------------------
;;; Example 1: Basic price calculation with VAT
;;; ---------------------------------------------------------------------------

(def vat-rate-standard 0.23M)
(def vat-rate-reduced  0.08M)

(defn add-vat
  "Adds VAT to net price."
  ([net-price]
   (add-vat net-price vat-rate-standard))
  ([net-price rate]
   (let [vat (m/mul net-price rate)]
     {:net   net-price
      :vat   vat
      :gross (m/add net-price vat)})))

(comment
  (add-vat #money[249.99 PLN])
  ;; => {:net   #money[249.99 PLN]
  ;;     :vat   #money[57.50 PLN]
  ;;     :gross #money[307.49 PLN]}

  ;; With reduced rate (e.g., books)
  (add-vat #money[49.90 PLN] vat-rate-reduced)
  ;; => {:net   #money[49.90 PLN]
  ;;     :vat   #money[3.99 PLN]
  ;;     :gross #money[53.89 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 2: Shopping cart with multiple products
;;; ---------------------------------------------------------------------------

(defn product
  "Creates a product with price."
  [name net-price quantity & {:keys [vat] :or {vat vat-rate-standard}}]
  {:name     name
   :price    net-price
   :quantity quantity
   :vat      vat})

(defn line-item-value
  "Calculates line item value (net, VAT, gross)."
  [{:keys [price quantity vat]}]
  (let [net     (m/mul price quantity)
        vat-amt (m/mul net vat)]
    {:net   net
     :vat   vat-amt
     :gross (m/add net vat-amt)}))

(defn cart-summary
  "Summarizes all cart items."
  [products]
  (let [items (map line-item-value products)]
    {:items items
     :total {:net   (apply m/add (map :net items))
             :vat   (apply m/add (map :vat items))
             :gross (apply m/add (map :gross items))}}))

(comment
  (def my-cart
    [(product "Laptop"   #money[3999.00 PLN] 1)
     (product "Mouse"    #money[129.00 PLN]  2)
     (product "Keyboard" #money[249.00 PLN]  1)
     (product "Book"     #money[59.90 PLN]   3 :vat vat-rate-reduced)])

  (cart-summary my-cart)
  ;; => {:items [...]
  ;;     :total {:net   #money[4685.70 PLN]
  ;;             :vat   #money[1028.24 PLN]
  ;;             :gross #money[5713.94 PLN]}}
  )

;;; ---------------------------------------------------------------------------
;;; Example 3: Payment gateway commission
;;; ---------------------------------------------------------------------------

(def gateway-fees
  "Fee structures for popular payment gateways."
  {:stripe     {:percent 0.029M :fixed #money[1.00 PLN]}
   :payu       {:percent 0.019M :fixed #money[0.50 PLN]}
   :przelewy24 {:percent 0.015M :fixed #money[0.00 PLN]}})

(defn calculate-fee
  "Calculates payment gateway fee."
  [gross-amount gateway]
  (let [{:keys [percent fixed]} (get gateway-fees gateway)]
    (m/add (m/mul gross-amount percent) fixed)))

(defn net-payout
  "Calculates net payout after gateway fee deduction."
  [gross-amount gateway]
  (let [fee (calculate-fee gross-amount gateway)]
    {:gross   gross-amount
     :fee     fee
     :payout  (m/sub gross-amount fee)}))

(comment
  (net-payout #money[1000.00 PLN] :stripe)
  ;; => {:gross  #money[1000.00 PLN]
  ;;     :fee    #money[30.00 PLN]
  ;;     :payout #money[970.00 PLN]}

  (net-payout #money[1000.00 PLN] :payu)
  ;; => {:gross  #money[1000.00 PLN]
  ;;     :fee    #money[19.50 PLN]
  ;;     :payout #money[980.50 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 4: Percentage and fixed discounts
;;; ---------------------------------------------------------------------------

(defn apply-percentage-discount
  "Applies percentage discount to amount."
  [amount discount-percent]
  (let [discount (m/mul amount discount-percent)]
    {:before-discount amount
     :discount        discount
     :after-discount  (m/sub amount discount)}))

(defn apply-fixed-discount
  "Applies fixed amount discount (coupon)."
  [amount discount-amount]
  (let [after (m/sub amount discount-amount)]
    {:before-discount amount
     :discount        discount-amount
     :after-discount  (if (m/is-neg? after)
                        (m/of (:currency amount) 0)
                        after)}))

(comment
  ;; 15% discount
  (apply-percentage-discount #money[299.99 PLN] 0.15M)
  ;; => {:before-discount #money[299.99 PLN]
  ;;     :discount        #money[45.00 PLN]
  ;;     :after-discount  #money[254.99 PLN]}

  ;; 50 PLN coupon
  (apply-fixed-discount #money[299.99 PLN] #money[50.00 PLN])
  ;; => {:before-discount #money[299.99 PLN]
  ;;     :discount        #money[50.00 PLN]
  ;;     :after-discount  #money[249.99 PLN]}

  ;; Coupon larger than amount - no negative price
  (apply-fixed-discount #money[30.00 PLN] #money[50.00 PLN])
  ;; => {:after-discount #money[0.00 PLN]}
  )

;;; ---------------------------------------------------------------------------
;;; Example 5: Installment calculation (simplified)
;;; ---------------------------------------------------------------------------

(defn calculate-installments
  "Splits amount into n equal installments."
  [amount num-installments]
  (m/distribute amount num-installments))

(comment
  ;; 1000 PLN in 3 installments
  (calculate-installments #money[1000.00 PLN] 3)
  ;; => [#money[333.34 PLN] #money[333.33 PLN] #money[333.33 PLN]]

  ;; Verify sum
  (apply m/add (calculate-installments #money[1000.00 PLN] 3))
  ;; => #money[1000.00 PLN]
  )

;;; ---------------------------------------------------------------------------
;;; Example 6: Free shipping threshold
;;; ---------------------------------------------------------------------------

(def free-shipping-threshold #money[200.00 PLN])
(def standard-shipping-cost  #money[14.99 PLN])

(defn calculate-shipping
  "Calculates shipping cost considering free shipping threshold."
  [cart-value]
  (if (m/ge? cart-value free-shipping-threshold)
    {:shipping              (m/of :PLN 0)
     :needed-for-free       nil}
    {:shipping              standard-shipping-cost
     :needed-for-free       (m/sub free-shipping-threshold cart-value)}))

(comment
  (calculate-shipping #money[150.00 PLN])
  ;; => {:shipping #money[14.99 PLN]
  ;;     :needed-for-free #money[50.00 PLN]}

  (calculate-shipping #money[250.00 PLN])
  ;; => {:shipping #money[0.00 PLN]
  ;;     :needed-for-free nil}
  )

;;; ---------------------------------------------------------------------------
;;; Example 7: Complete e-commerce transaction
;;; ---------------------------------------------------------------------------

(defn process-order
  "Processes complete order: products, discount, shipping, gateway fee."
  [{:keys [products discount-percent gateway]}]
  (let [cart        (cart-summary products)
        gross       (get-in cart [:total :gross])
        after-disc  (if discount-percent
                      (:after-discount (apply-percentage-discount gross discount-percent))
                      gross)
        ship-info   (calculate-shipping after-disc)
        to-pay      (m/add after-disc (:shipping ship-info))
        fee         (calculate-fee to-pay gateway)]
    {:cart         cart
     :discount     (when discount-percent (m/sub gross after-disc))
     :shipping     (:shipping ship-info)
     :to-pay       to-pay
     :gateway-fee  fee
     :net-payout   (m/sub to-pay fee)}))

(comment
  (process-order
   {:products [(product "Laptop" #money[2999.00 PLN] 1)
               (product "Case"   #money[99.00 PLN] 1)]
    :discount-percent 0.10M
    :gateway :stripe})
  ;; => {:cart {...}
  ;;     :discount #money[381.08 PLN]
  ;;     :shipping #money[0.00 PLN]  ; above threshold
  ;;     :to-pay #money[3429.72 PLN]
  ;;     :gateway-fee #money[100.46 PLN]
  ;;     :net-payout #money[3329.26 PLN]}
  )
