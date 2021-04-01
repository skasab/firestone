(ns firestone.core
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.random :refer [random-nth
                                  take-n-random]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [add-dead-minion
                                         add-minion-buff
                                         add-card-buff
                                         add-card-to-deck
                                         add-card-to-hand
                                         add-minion-to-board
                                         charge?
                                         create-card
                                         create-empty-state
                                         create-game
                                         create-hero
                                         create-minion
                                         divine-shield?
                                         generate-new-minion-id
                                         get-minion-buffs
                                         get-card
                                         get-deck
                                         get-enemy-minions
                                         get-hand
                                         get-hero
                                         get-heroes
                                         get-minion
                                         get-minions
                                         get-owner-id
                                         get-player-id-in-turn
                                         inc-attacks-performed-minion
                                         minion-exists?
                                         next-minion-position
                                         remove-card-from-deck
                                         remove-card-from-hand
                                         remove-divine-shield
                                         remove-minion
                                         remove-minions
                                         replace-minion
                                         taunt?
                                         update-minion
                                         windfury?]]
            [firestone.random :refer [get-random-enemy-minion-id
                                      get-n-random-enemy-minion-ids
                                      get-random-friendly-minion-id
                                      get-n-random-friendly-minion-ids
                                      get-random-minion-id
                                      get-n-random-minion-ids]]))


(defn get-characters
  "Returns all of the characters on the board."
  [state]
  (concat (get-minions state)
          (get-heroes state)))

(defn get-character
  "Returns the character with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (get-character "h1")
                    (:name))
                "Carl")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-character "m")
                    (:name))
                "Mio"))}
  [state id]
  (or (some (fn [m] (when (= (:id m) id) m))
            (get-minions state))
      (some (fn [h] (when (= (:id h) id) h))
            (get-heroes state))))

(defn same-owner?
  "Returns whether or not two characters are owned by the same player."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                            (create-minion "Emil" :id "e")]}])
                   (same-owner? "m" "e")))
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Emil" :id "e")]}])
                       (same-owner? "m" "e"))))}

  ([entity-1 entity-2]
   (= (:owner-id entity-1) (:owner-id entity-2)))
  ([state id-1 id-2]
   (same-owner? (get-character state id-1)
                (get-character state id-2))))

(defn get-max-health
  "Returns the max health of the character."
  {:test (fn []
           ; The health of minions
           (is= (-> (create-minion "Ronja")
                    (get-max-health))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (add-minion-buff "e" {:attack 2})
                    (get-minion "e")
                    (get-max-health))
                7))}
  ([character]
   (let [definition (get-definition character)
         health-from-buffs (->> (:buffs character)
                                (map :health)
                                (remove nil?)
                                (apply +))]
     (+ (:health definition) health-from-buffs)))
  ([state id]
   (get-max-health (get-character state id))))

(defn get-health
  "Returns the health of the character."
  {:test (fn []
           ; The health of minions
           (is= (-> (create-minion "Ronja")
                    (get-health))
                2)
           (is= (-> (create-minion "Ronja" :damage-taken 1)
                    (get-health))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-health "m"))
                2)

           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (add-minion-buff "e" {:attack 2})
                    (get-minion "e")
                    (get-health))
                7)

           ; The health of heroes
           (is= (-> (create-hero "Carl")
                    (get-health))
                30)
           (is= (-> (create-hero "Carl" :damage-taken 2)
                    (get-health))
                28)
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (get-health "h1"))
                30))}
  ([character]
   {:pre [(map? character) (contains? character :damage-taken)]}
   (let [definition (get-definition character)
         health-from-buffs (->> (:buffs character)
                                (map :health)
                                (remove nil?)
                                (apply +))]
     (- (+ (:health definition) health-from-buffs)
        (:damage-taken character))))
  ([state id]
   (get-health (get-character state id))))

(defn get-attack
  "Returns the attack of the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-attack "m"))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-minion "m")
                    (get-attack))
                1))}

  ([minion]
   (let [definition (get-definition (:name minion))
         attack-from-buffs (->> (:buffs minion)
                                (map :attack)
                                (remove nil?)
                                (apply +))]
     (+ attack-from-buffs (:attack definition))))
  ([state id]
   (get-attack (get-character state id))))

(defn get-card-mana-cost
  "Returns the mana cost of the given card."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (get-card-mana-cost "m"))
                1)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (add-card-buff "e" {:mana-cost -1 :health 2} :hand)
                    (add-card-buff "e" {:mana-cost -1} :hand)
                    (get-card "e")
                    (get-card-mana-cost))
                2)
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (add-card-buff "e" {:health 2} :hand)
                    (get-card "m")
                    (get-card-mana-cost))
                1))}
  ([card]
   (let [definition (get-definition (:name card))
         mana-cost-from-buffs (->> (:buffs card)
                                   (map :mana-cost)
                                   (remove nil?)
                                   (apply +))]
     (+ mana-cost-from-buffs (:mana-cost definition))))
  ([state id]
   (get-card-mana-cost (get-card state id))))

(defn get-minion-card-attack
  "Returns the attack value of the specified minion card (base plus any buffs)."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (get-minion-card-attack "m"))
                1)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (add-card-buff "e" {:attack 1 :health 2} :hand)
                    (add-card-buff "e" {:attack 1} :hand)
                    (get-card "e")
                    (get-minion-card-attack))
                4)
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (add-card-buff "e" {:health 2} :hand)
                    (get-card "m")
                    (get-minion-card-attack))
                1))}
  ([card]
   (let [definition (get-definition (:name card))
         attack-from-buffs (->> (:buffs card)
                                (map :attack)
                                (remove nil?)
                                (apply +))]
     (+ attack-from-buffs (:attack definition))))
  ([state id]
   (get-minion-card-attack (get-card state id))))

(defn get-minion-card-health
  "Returns the health of the given minion card (base plus any buffs)."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (get-minion-card-health "m"))
                2)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (add-card-buff "e" {:health 1 :attack 2} :hand)
                    (add-card-buff "e" {:health 1} :hand)
                    (get-card "e")
                    (get-minion-card-health))
                7)
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (add-card-buff "e" {:attack 2} :hand)
                    (get-card "m")
                    (get-minion-card-health))
                2))}
  ([card]
   (let [definition (get-definition (:name card))
         health-from-buffs (->> (:buffs card)
                                (map :health)
                                (remove nil?)
                                (apply +))]
     (+ health-from-buffs (:health definition))))
  ([state id]
   (get-minion-card-health (get-card state id))))

(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]
                                :minion-ids-summoned-this-turn ["m"])
                   (sleepy? "m")))
           (is-not (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "a")]}]
                                    :minion-ids-summoned-this-turn ["a"])
                       (sleepy? "a")))
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                       (sleepy? "m"))))}
  [state id]
  (and (not (charge? state (get-minion state id)))
       (seq-contains? (:minion-ids-summoned-this-turn state) id)))

(comment (defn minion-on-board?
           {:test (fn []
                    (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                             (minion-on-board? "m"))
                         true)
                    (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                             (minion-on-board? "m"))
                         false))}

           [state minion-id]
           (not= (get-minion state minion-id) nil)))

(defn valid-attack?
  "Checks if the attack is valid."
  {:test (fn []
           ; Should be able to attack an enemy minion
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                 {:minions [(create-minion "Ronja" :id "r")]}])
                   (valid-attack? "p1" "m" "r")))
           ; Should be able to attack an enemy hero
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                   (valid-attack? "p1" "m" "h2")))
           ; Should be able to attack an enemy minion with taunt
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                 {:minions [(create-minion "Elisabeth" :id "e")]}])
                   (valid-attack? "p1" "m" "e")))
           ; Should be able to attack twice with windfury
           (is (-> (create-game [{:minions [(create-minion "Rasmus" :id "r")]}
                                 {:minions [(create-minion "Mio" :id "m")]}])
                   (inc-attacks-performed-minion "r")
                   (valid-attack? "p1" "r" "m")))
           ; Should not be able to attack your own minions
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                                (create-minion "Ronja" :id "r")]}])
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack if your definition prevents it
           (is-not (-> (create-game [{:minions [(create-minion "Alfred" :id "a")]}])
                       (valid-attack? "p1" "a" "h2")))
           ; Should not be able to attack if your attack strength is 0
           (is-not (-> (create-game [{:minions [(create-minion "ZeroAttack" :id "z")]}])
                       (valid-attack? "p1" "z" "h2")))
           ; Should not be able to attack if it is not your turn
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Ronja" :id "r")]}]
                                    :player-id-in-turn "p2")
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack if you are sleepy
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Ronja" :id "r")]}]
                                    :minion-ids-summoned-this-turn ["m"])
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack an enemy minion without taunt if another minion has taunt
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Kato" :id "k")
                                                (create-minion "Elisabeth" :id "e")]}])
                       (valid-attack? "p1" "m" "k")))
           ; Should not be able to attack if you already attacked this turn
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m" :attacks-performed-this-turn 1)]}
                                     {:minions [(create-minion "Ronja" :id "r")]}])
                       (valid-attack? "p1" "m" "r"))))}

  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)
        enemy-minions (get-enemy-minions state player-id)]
    (and attacker
         target
         (or (taunt? state target)
             (not (some (fn [x] (taunt? state x)) enemy-minions)))
         (= (:player-id-in-turn state) player-id)
         (if (windfury? state attacker)
           (< (:attacks-performed-this-turn attacker) 2)
           (< (:attacks-performed-this-turn attacker) 1))
         (nil? (:cannot-attack (get-definition (:name attacker))))
         (pos? (get-attack attacker))
         (not (sleepy? state attacker-id))
         (not= (:owner-id attacker) (:owner-id target)))))

(defn trigger-action-on-minions
  "Triggers actions for various conditions (such as the end of a turn, or a minion taking damage, etc.)."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Ida" :id "i")]}
                                   {:minions [(create-minion "Ronja" :id "r")]}]) $
                     (trigger-action-on-minions $ :on-damage)
                     (taunt? $ (get-minion $ "i"))))
           (is= (as-> (create-game [{:minions [(create-minion "Pippi" :id "p")
                                               (create-minion "Kato" :id "k1")]}
                                    {:minions [(create-minion "Kato" :id "k2")
                                               (create-minion "Kato" :id "k3")]}]) $
                      (trigger-action-on-minions $ :on-end-turn)
                      (count (get-minions $)))
                1)
           (is= (as-> (create-game [{:minions [(create-minion "Skrallan" :id "s")
                                               (create-minion "Elisabeth" :id "e")]}]) $
                      (trigger-action-on-minions $ :on-divine-shield-loss "e")
                      (get-attack (get-minion $ "s")))
                4))}

  ([state trigger-keyword target-id]
   (let [triggered-characters (reduce (fn [list m]
                                        (if (contains? (apply merge (:buffs m)) trigger-keyword)
                                          (conj list m)
                                          list))
                                      []
                                      (get-minions state))
         pairs-of-minion-names-and-ids (reduce (fn [list-of-pairs c]
                                                 (concat list-of-pairs
                                                         (reduce (fn [list-of-triggers b]
                                                                   (if (contains? b trigger-keyword)
                                                                     (conj list-of-triggers (vector (get b trigger-keyword) (:id c)))
                                                                     list-of-triggers))
                                                                 []
                                                                 (:buffs c))))
                                               []
                                               triggered-characters)]
     (as-> state $
           (reduce (fn [acc-state minion-name-id-pair]
                     (let [minion-name (first minion-name-id-pair)
                           minion-id (last minion-name-id-pair)
                           effect-fn (get (get-definition minion-name) trigger-keyword)]
                       (effect-fn acc-state {:minion-id minion-id :target-id target-id})))
                   $
                   pairs-of-minion-names-and-ids))))
  ([state trigger-keyword]
   (trigger-action-on-minions state trigger-keyword nil)))

(defn tick-down-temporary-buffs
  "Ticks down temporary buffs and removes them if duration is 0"
  {:test (fn []
           ; Should tick down temporary buff
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2 :duration 2})
                    (tick-down-temporary-buffs)
                    (get-minion "e")
                    (:buffs))
                [{:attack 2 :health 2 :duration 1}])
           ; Should tick down all temporary buffs
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2 :duration 2})
                    (add-minion-buff "e" {:taunt true :duration 3})
                    (tick-down-temporary-buffs)
                    (get-minion "e")
                    (:buffs))
                [{:attack 2 :health 2 :duration 1} {:taunt true :duration 2}])
           ; Should not affect permanent buffs
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:divine-shield true})
                    (tick-down-temporary-buffs)
                    (get-minion "e")
                    (:buffs))
                [{:divine-shield true}])
           ; Should remove buffs with duration 0
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:divine-shield true})
                    (add-minion-buff "e" {:attack 4 :health 5 :duration 1})
                    (tick-down-temporary-buffs)
                    (get-minion "e")
                    (:buffs))
                [{:divine-shield true}]))}
  [state]
  (let [minions (get-minions state)]
    (reduce (fn [acc-state m]
              (let [buffs (get-minion-buffs acc-state m)
                    new-buffs (reduce (fn [acc-buffs b]
                                        (if (contains? b :duration)
                                          (as-> (update b :duration (fn [old-duration] (- old-duration 1))) buff-with-duration-reduced
                                                (if-not (>= 0 (:duration buff-with-duration-reduced))
                                                  (conj acc-buffs buff-with-duration-reduced)
                                                  acc-buffs))
                                          (conj acc-buffs b)))
                                      []
                                      buffs)]
                (update-minion acc-state (:id m) :buffs new-buffs)))
            state
            minions)))

(defn update-hero-damage
  "Updates the damage taken of the hero with the given id with the specified damage."
  {:test (fn []
           (is= (-> (create-game)
                    (update-hero-damage "h1" 4)
                    (get-health "h1"))
                26)
           (is= (-> (create-game)
                    (update-hero-damage "h1" inc)
                    (get-health "h1"))
                29)
           (is= (-> (create-game)
                    (update-hero-damage "h1" 0)
                    (get-health "h1"))
                30))}

  [state hero-id fn-or-value]
  (let [player-id (:owner-id (get-hero state hero-id))]
    (if (fn? fn-or-value)
      (update-in state [:players player-id :hero :damage-taken] fn-or-value)
      (update-in state [:players player-id :hero :damage-taken] (fn [old-damage] (+ old-damage fn-or-value))))))

(defn update-times-hero-power-used
  "Updates the times the hero power is used in the hero map."
  {:test (fn []
           (is= (-> (create-game)
                    (update-times-hero-power-used "h1" 4)
                    (get-hero "h1")
                    (:times-hero-power-used))
                4)
           (is= (-> (create-game)
                    (update-times-hero-power-used "h1" inc)
                    (get-hero "h1")
                    (:times-hero-power-used))
                1)
           (is= (-> (create-game)
                    (update-times-hero-power-used "h1" 0)
                    (get-hero "h1")
                    (:times-hero-power-used))
                0))}

  [state hero-id fn-or-value]
  (let [player-id (:owner-id (get-hero state hero-id))]
    (if (fn? fn-or-value)
      (update-in state [:players player-id :hero :times-hero-power-used] fn-or-value)
      (update-in state [:players player-id :hero :times-hero-power-used] (fn [old-val] (+ old-val fn-or-value))))))

(defn check-if-minion-dead
  "Checks whether the minion with the given id still has health remaining."
  {:test (fn []
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                       (check-if-minion-dead "m")))
           (is (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                   (update-minion "e" :damage-taken 7)
                   (check-if-minion-dead "e"))))}

  [state minion-id]
  (<= (get-health state minion-id) 0))

(defn deathrattle
  "Does the given deathrattle."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}])
                    (deathrattle "m")
                    (get-minions "p1")
                    (count))
                2))}
  [state minion-id]
  (let [minion (get-minion state minion-id)
        deathrattle-names (->> (:buffs minion)
                               (map (fn [{deathrattle-name :deathrattle}] deathrattle-name))
                               (remove nil?))
        deathrattle-fns (->> deathrattle-names
                             (map get-definition)
                             (map :deathrattle))
        player-id (:owner-id minion)]
    (reduce (fn [state deathrattle-fn]
              (or (deathrattle-fn state {:player-id player-id})
                  state))
            state
            deathrattle-fns)))

(defn remove-given-minion-if-dead
  "Removes minion with the given id from the board if they have no health left."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion "m" :damage-taken 2)
                    (remove-given-minion-if-dead "m")
                    (get-minions))
                [])
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (update-minion "e" :damage-taken 1)
                    (remove-given-minion-if-dead "e")
                    (get-minions)
                    (count))
                1)
           ; Deathrattle should apply
           (is= (-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}])
                    (update-minion "m" :damage-taken 10)
                    (remove-given-minion-if-dead "m")
                    (get-minions)
                    (count))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion "m" :damage-taken 6)
                    (remove-given-minion-if-dead "m")
                    (get-minions))
                []))}
  [state minion-id]
  (if (check-if-minion-dead state minion-id)
    (as-> state $
          (deathrattle $ minion-id)
          (trigger-action-on-minions $ :when-minion-dies)
          (remove-minion $ minion-id))
    state))

(defn update-minion-damage
  "Updates the damage taken of the minion with the given id with the specified damage."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion-damage "m" 1)
                    (get-health "m"))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Ronja" :id "r")]}])
                    (update-minion-damage "r" 0)
                    (get-health "r"))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (update-minion-damage "e" inc)
                    (get-health "e"))
                4)
           (is (-> (create-game [{:minions [(create-minion "Ida" :id "i")]}])
                   (update-minion-damage "i" inc)
                   (get-minion "i")
                   (taunt?))))}

  [state minion-or-id fn-or-value]
  (let [minion (if (string? minion-or-id)
                 (get-minion state minion-or-id)
                 minion-or-id)
        minion-id (:id minion)]
    (-> state
        (update-minion minion-id :damage-taken fn-or-value)
        (trigger-action-on-minions :on-damage)
        (remove-given-minion-if-dead minion-id))))          ;)

(defn get-entity-type
  "Returns the type of character with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-entity-type "m"))
                :minion)
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (get-entity-type "h1"))
                :hero))}

  [state character-id]
  (-> state
      (get-character character-id)
      (:entity-type)))

(defn minion?
  "Returns whether or not the character with the given id is a minion or not."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (minion? "m"))
                true)
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (minion? "h1"))
                false))}

  ([character]
   (= (:entity-type character) :minion))
  ([state character-id]
   (= (get-entity-type state character-id) :minion)))

(defn hero?
  "Returns whether or not the character with the given id is a hero or not."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (hero? "m"))
                false)
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (hero? "h1"))
                true))}

  ([character]
   (= (:entity-type character) :hero))
  ([state character-id]
   (= (get-entity-type state character-id) :hero)))

(defn secret?
  "Returns whether or not the card is a secret or not."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")]}])
                    (secret? "r"))
                false)
           (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")]}])
                    (secret? "v"))
                true)
           (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")]}])
                    (get-card "v")
                    (secret?))
                true))}

  ([card]
   (= (:sub-type (get-definition card)) :secret))
  ([state card-id]
   (= (:sub-type (get-definition (get-card state card-id))) :secret)))

(defn damage
  "Determines whether the character taking damage is a hero or minion and calls the appropriate function with the specified amount of damage."
  {:test (fn []
           (is= (-> (create-game)
                    (damage "h1" 5)
                    (get-health "h1"))
                25)
           (is= (-> (create-game)
                    (damage "h1" inc)
                    (get-health "h1"))
                29)
           (is= (-> (create-game)
                    (damage "h1" 5)
                    (damage "h1" 5)
                    (get-health "h1"))
                20)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (damage "e" 3)
                    (get-health "e"))
                2)
           ; Should not damage divine shield holder
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (damage "e" 10)
                    (get-health "e"))
                1)
           ; Should remove divine shield
           (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                       (damage "e" 1)
                       (get-minion "e")
                       (divine-shield?))))}

  [state character-or-id amount-or-fn]
  (let [character (if (string? character-or-id)
                    (get-character state character-or-id)
                    character-or-id)
        character-id (:id character)
        damage-fn (fn [x] (+ x amount-or-fn))]

    (if (minion? character)
      (if (divine-shield? state character)
        (as-> state $
              (remove-divine-shield $ character-id)
              (trigger-action-on-minions $ :on-divine-shield-loss character-id))
        (if (fn? amount-or-fn)
          (update-minion-damage state character-id amount-or-fn)
          (update-minion-damage state character-id damage-fn)))
      (if (fn? amount-or-fn)
        (update-hero-damage state character-id amount-or-fn)
        (update-hero-damage state character-id damage-fn)))))

(defn damage-all-in-list
  "Damages all characters in a collection by the specified amount."
  [state characters amount]
  (reduce (fn [state c]
            (damage state c amount))
          state
          characters))

(defn max-minions?
  "Returns whether or not a given player has reached the maximum amount of minions. (Cannot have more)"
  {:test (fn []
           (is-not (-> (create-game)
                       (max-minions? "p1")))
           (is (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m0") (create-minion "Mio" :owner-id "p1" :id "m1") (create-minion "Mio" :owner-id "p1" :id "m2") (create-minion "Mio" :owner-id "p1" :id "m3") (create-minion "Mio" :owner-id "p1" :id "m4") (create-minion "Mio" :owner-id "p1" :id "m5") (create-minion "Mio" :owner-id "p1" :id "m6")]}])
                   (max-minions? "p1"))))}
  [state player-id]
  (>= (count (get-minions state player-id)) 7))

(defn get-fatigue
  "Returns the current fatigue value for given player."
  {:test (fn []
           (is= (-> (create-game)
                    (get-fatigue "p1"))
                0))}
  [state player-id]
  (get-in state [:players player-id :fatigue]))

(defn inc-fatigue
  "Increments the given player's fatigue value."
  {:test (fn []
           (is= (-> (create-game)
                    (inc-fatigue "p1")
                    (get-fatigue "p1"))
                1)
           (is= (-> (create-game)
                    (inc-fatigue "p1")
                    (inc-fatigue "p1")
                    (get-fatigue "p1"))
                2))}
  [state player-id]
  (update-in state [:players player-id :fatigue] inc))

(defn get-hero-id-from-player
  "Returns the id of the given player's hero."
  {:test (fn []
           (is= (-> (create-game)
                    (get-hero-id-from-player "p1"))
                "h1"))}
  [state player-id]
  (get-in state [:players player-id :hero :id]))

(defn apply-fatigue
  "Applies damage corresponding to the given player's fatigue value to their hero."
  {:test (fn []
           (is= (-> (create-game)
                    (apply-fatigue "p1")
                    (get-health "h1"))
                30))}

  [state player-id]
  (let [fatigued (get-fatigue state player-id)
        hero-id (get-hero-id-from-player state player-id)]
    (update-hero-damage state hero-id fatigued)))

(defn fatigue
  "Increments the given player's fatigue value and the deals that amount of damage to their hero."
  {:test (fn []
           (is= (-> (create-game)
                    (fatigue "p1")
                    (get-health "h1"))
                29)
           (is= (-> (create-game)
                    (fatigue "p1")
                    (fatigue "p1")
                    (fatigue "p1")
                    (get-health "h1"))
                24))}

  [state player-id]
  (-> state
      (inc-fatigue player-id)
      (apply-fatigue player-id)))

(defn max-hand?
  "Returns whether or not the player possesses the maximum amount of cards in their hand. (Cannot get more)"
  {:test (fn []
           (is-not (-> (create-game)
                       (max-hand? "p1")))
           (is (-> (create-game [{:hand (repeat 10 "Emil")}])
                   (max-hand? "p1"))))}
  [state player-id]
  (>= (count (get-hand state player-id)) 10))

(defn draw-card
  "Has the given player remove the top card from their deck and add it to their hand if possible, factoring in fatigue if necessary."
  {:test (fn []
           (is= (-> (create-game)
                    (add-card-to-deck "p1" "Mio")
                    (add-card-to-deck "p1" "Kato")
                    (draw-card "p1")
                    (get-deck "p1")
                    (count))
                1)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e")]}])
                    (add-card-to-deck "p1" "Mio")
                    (draw-card "p1")
                    (get-hand "p1")
                    (count))
                10)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e") (create-card "Emil" :id "e")]}])
                    (add-card-to-deck "p1" "Mio")
                    (draw-card "p1")
                    (get-deck "p1")
                    (count))
                0)
           (is= (-> (create-game)
                    (add-card-to-deck "p1" "Mio")
                    (draw-card "p1")
                    (draw-card "p1")
                    (get-hand "p1")
                    (count))
                1)
           (is= (-> (create-game)
                    (draw-card "p1")
                    (get-health "h1"))
                29)
           (is= (-> (create-game)
                    (draw-card "p1")
                    (draw-card "p1")
                    (get-health "h1"))
                27))}

  [state player-id]
  (let [deck (get-deck state player-id)]
    (if (empty? deck)
      (fatigue state player-id)
      (let [card (first deck)
            card-id (:id card)]
        (if (max-hand? state player-id)
          (remove-card-from-deck state player-id card-id)
          (-> state
              (remove-card-from-deck player-id card-id)
              (add-card-to-hand player-id card)))))))

(defn add-to-minion-ids-summoned-this-turn
  "Adds the given minion-id to the list of minions summoned this turn."
  {:test (fn []
           (is= (-> (create-game)
                    (add-to-minion-ids-summoned-this-turn "e")
                    (:minion-ids-summoned-this-turn)
                    (count))
                1))}

  [state minion-id]
  (->> (conj (get state :minion-ids-summoned-this-turn) minion-id)
       (assoc state :minion-ids-summoned-this-turn)))

(defn summon
  "Puts a minion on the board (if room is available), without requiring mana, adding it to the list of minions summoned this turn."
  {:test (fn []
           (is= (-> (create-game)
                    (summon "p1" "Emil")
                    (get-minions "p1")
                    (count))
                1))}

  [state player-id minion-name]
  (if (max-minions? state player-id)
    state
    (let [[state new-minion-id] (generate-new-minion-id state)
          new-minion (create-minion minion-name :id new-minion-id)]
      (-> state
          (add-minion-to-board player-id
                               new-minion
                               (next-minion-position state player-id))
          (add-to-minion-ids-summoned-this-turn new-minion-id)
          (trigger-action-on-minions :on-summon)))))

(defn silence-minion
  "Silences the specified minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (silence-minion "e")
                    (get-minion-buffs "e"))
                [{:silenced true}])
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (add-minion-buff "e" {:attack 2})
                    (silence-minion "e")
                    (get-minion-buffs "e"))
                [{:silenced true}]))}
  [state minion-or-id]
  (let [minion-id (if (string? minion-or-id)
                    minion-or-id
                    (:id minion-or-id))]
    (-> state
        (update-minion minion-id :abilities [])
        (update-minion minion-id :buffs [{:silenced true}]))))




































