(ns firestone.random
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.random :refer [random-nth
                                  take-n-random]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [
                                         create-game
                                         create-hero
                                         create-minion
                                         get-all-secrets
                                         get-minion-buffs
                                         get-card
                                         get-deck
                                         get-enemy-minions
                                         get-enemy-hero-by-player-id
                                         get-enemy-secrets
                                         get-hand
                                         get-hero
                                         get-hero-by-player-id
                                         get-heroes
                                         get-minion
                                         get-minions
                                         get-secrets]]))

(defn random
  "Given the state and a collection you will get a tuple back containing the new state and a random element from the collection."
  {:test (fn []
           (is= (random {:seed 0} [1 2 3])
                [{:seed 1} 1])
           (is= (random {:seed 0} 3 [1 2 3 4 5 6])
                [{:seed 1130298060341683} [1 3 5]]))}
  ([state coll]
   (let [[new-seed element] (random-nth (:seed state) coll)]
     [(assoc state :seed new-seed) element]))
  ([state n coll]
   (let [[new-seed elements] (take-n-random (:seed state) n coll)]
     [(assoc state :seed new-seed) elements])))

(defn get-random-according-to-fn
  "Returns random member of collection conforming to given function."
  [state coll spec-fn]
  (let [valid-targets (filter (fn [element] (spec-fn element)) coll)]
    (random state valid-targets)))

(defn get-random-friendly-minion-id
  "Gets a random minion id of from the player's side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions ["Kato" "Alfred" "Jonatan"]}])
                    (get-random-friendly-minion-id "p1")
                    (second))
                "m")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions ["Kato" "Alfred" "Jonatan"]}]
                                 :seed 10)
                    (get-random-friendly-minion-id "p1")
                    (second))
                "e"))}
  [state player-id]
  (let [[state minion] (random state (get-minions state player-id))]
    [state (:id minion)]))

(defn get-n-random-friendly-minion-ids
  "Gets n random minion ids from the current player's side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions ["Kato" "Alfred" "Jonatan"]}])
                    (get-n-random-friendly-minion-ids "p1" 2)
                    (second))
                ["m" "r"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions ["Kato" "Alfred" "Jonatan"]}])
                    (get-n-random-friendly-minion-ids "p1" 5)
                    (second))
                ["m" "r" "e"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions ["Kato" "Alfred" "Jonatan"]}])
                    (get-n-random-friendly-minion-ids "p1" 4)
                    (second))
                ["m" "r" "e"]))}
  [state player-id number]
  (let [[state minions] (random state number (get-minions state player-id))]
    [state (map :id minions)]))

(defn get-random-enemy-minion-id
  "Gets a random minion id from the opposing player's side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Emil" :id "m3")
                                             (create-minion "Ronja" :id "m5")]}])
                    (get-random-enemy-minion-id "p2")
                    (second))
                "m1")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Emil" :id "m3")
                                             (create-minion "Ronja" :id "m5")]}]
                                 :seed 10)
                    (get-random-enemy-minion-id "p2")
                    (second))
                "m3"))}
  [state player-id]
  (let [minions (get-enemy-minions state player-id)
        [state minion] (random state minions)]
    [state (:id minion)]))

(defn get-n-random-enemy-minion-ids
  "Get n random minion ids from the enemy side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-n-random-enemy-minion-ids "p1" 2)
                    (second))
                ["k" "j"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                 :seed 10)
                    (get-n-random-enemy-minion-ids "p1" 2)
                    (second))
                ["a" "j"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-n-random-enemy-minion-ids "p1" 5)
                    (second))
                ["k" "j" "a"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-n-random-enemy-minion-ids "p1" 4)
                    (second))
                ["k" "j" "a"]))}
  [state player-id number]
  (let [[state minions] (random state number (get-enemy-minions state player-id))]
    [state (map :id minions)]))

(defn get-random-minion-id
  "Gets a random minion id from any side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-random-minion-id)
                    (second))
                "m")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                 :seed 10)
                    (get-random-minion-id)
                    (second))
                "a"))}
  [state]
  (let [minions (get-minions state)
        [state minion] (random state minions)]
    [state (:id minion)]))

(defn get-n-random-minion-ids
  "Gets n random minion ids from the game."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-n-random-minion-ids 2)
                    (second))
                ["m" "r"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                 :seed 10)
                    (get-n-random-minion-ids 2)
                    (second))
                ["a" "e"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-n-random-minion-ids 5)
                    (second))
                ["m" "r" "a" "k" "e"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}])
                    (get-n-random-minion-ids 4)
                    (second))
                ["m" "r" "a" "k"]))}
  [state number]
  (let [[state minions] (random state number (get-minions state))]
    [state (map :id minions)]))

(defn get-random-battlecry-target
  "Gets a random target in the game conforming to the spec of the given battlecry"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                  :seed 10)
                    (get-random-battlecry-target "p1" "Annika")
                    (:id))
                "a")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                 :seed 100)
                    (get-random-battlecry-target "p1" "Astrid"))
                nil)
           (is= (-> (create-game [{:minions [(create-minion "Uncle Nilsson" :id "un")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                 :seed -20)
                    (get-random-battlecry-target "p1" "Astrid")
                    (:id))
                "un")
           (is= (-> (create-game [{:minions [(create-minion "Uncle Nilsson" :id "un")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]}]
                                 :seed 1020)
                    (get-random-battlecry-target "p1" "Astrid")
                    (:id))
                "un"))}

  [state player-id battlecry-name]
  (let [definition (get-definition battlecry-name)
        target-type (:target-type definition)
        valid-target-fn (:valid-target-fn definition)
        possible-targets (when target-type
                           (cond
                             (= target-type :any-minion)
                             (get-minions state)

                             (= target-type :any-hero)
                             (get-heroes state)

                             (= target-type :any-secret)
                             (get-all-secrets state player-id)

                             (= target-type :friendly-minion)
                             (get-minions state player-id)

                             (= target-type :friendly-hero)
                             (get-hero-by-player-id state player-id)

                             (= target-type :friendly-secret)
                             (get-secrets state player-id)

                             (= target-type :enemy-minion)
                             (get-enemy-minions state player-id)

                             (= target-type :enemy-hero)
                             (get-enemy-hero-by-player-id state player-id)

                             (= target-type :enemy-secret)
                             (get-enemy-secrets state player-id)))

        [state target] (get-random-according-to-fn state possible-targets
                                                   (fn [t] (valid-target-fn state {:target-id (:id t)})))]
    (when target
      target)))




