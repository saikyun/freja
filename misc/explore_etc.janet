(varfn frame
  [dt]
  (debug-view state))

(comment
 #)
 (def people @{:unemployed 30
               :explorers 0
               :newbuilders 0
               :farmers 0
               :developers 0})

 (def planets @{:gas 0
                :earthlike 1
                :rocky 0})

 (def settlements @{:gas 0
                    :earthlike 1
                    :rocky 0})

 (def state @{:credits 10000
              :people people
              :planets planets
              :settlements settlements})

 #
 )

(put-in state [:stats :newbuilders :nof-needed] @{:rocky 10})
(put-in state [:stats :rocky :production] 100)
(put-in state [:stats :birthrate] 0.1)
(put state :prices @{:food 1
                     :space-ore 10})

### verbs
``
- retrain

- explore
-- solar system
-- planet
build
-- farms
-- homes
-- cafes
farm
-- food
-- tobacco
develop
-- improve rate of exploring, building, farming and development
``

(def chance-to-find-planet
  [[:earthlike 0.01] [:gas 0.03] [:rocky 0.08]])

(defn find-planet
  [tried]
  (reduce (fn [found [planet p]]
            (if (and (nil? found) (<= tried p))
              planet
              found))
          nil
          chance-to-find-planet))

(defn explore
  [state]
  (when (pos? (get-in state [:people :explorers]))
    (update-in state [:people :explorers] dec)
    (put state :just-found nil)
    (def tried (math/random))
    (when-let [planet (find-planet tried)]
      (update-in state [:planets planet] inc)
      (put state :just-found planet))
    (put state :last-try tried)))

(defn retrain
  [state from to]
  (when (pos? (get-in state [:people from]))
    (update-in state [:people from] dec)
    (update-in state [:people to] inc)))

(defn settle
  [state kind]
  (def nof-needed (get-in state [:stats :newbuilders :nof-needed kind]))
  (when (and (<= nof-needed
                 (get-in state [:people :newbuilders]))
             (> (get-in state [:planets kind])
                (get-in state [:settlements kind])))
    (update-in state [:people :newbuilders] - nof-needed)
    (update-in state [:settlements kind] inc)))

(defn babies-are-born
  [state]
  (let [newborn (-> (* 0.5 (get-in state [:people :unemployed])
                       (math/random)
                       (get-in state [:stats :birthrate]))
                    math/round)]
    (update-in state [:people :unemployed] + newborn)))

(defn pay-for-food
  [state]
  (let [peeps (+ ;(values (state :people)))
        food-cost (* peeps (get-in state [:prices :food]))]
    (update state :credits - food-cost)
    (put state :food-cost food-cost)))

(defn income
  [state]
  (let [income (* (get-in state [:settlements :rocky])
                  (get-in state [:stats :rocky :production])
                  (get-in state [:prices :space-ore]))]
    (update state :credits + income)
    (put state :income income)))

#(retrain state :unemployed :newbuilders)
#(explore state)
#(settle state :rocky)

(babies-are-born state)
(income state)
(pay-for-food state)

(string/format "%.4m" state)

# (string/format "%.4m" chance-to-find-planet)

