(ns firestone.core-api
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.core :refer [add-to-minion-ids-summoned-this-turn
                                    check-if-minion-dead
                                    damage
                                    draw-card
                                    get-attack
                                    get-card-mana-cost
                                    get-characters
                                    get-health
                                    get-hero-id-from-player
                                    hero?
                                    max-minions?
                                    minion?
                                    secret?
                                    tick-down-temporary-buffs
                                    trigger-action-on-minions
                                    valid-attack?
                                    update-times-hero-power-used]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [add-battlecry-name
                                         add-card-buff
                                         add-card-to-deck
                                         add-card-to-hand
                                         add-minion-buff
                                         add-minion-buffs
                                         add-minion-to-board
                                         add-secret
                                         create-card
                                         create-empty-state
                                         create-game
                                         create-hero
                                         create-minion
                                         divine-shield?
                                         generate-new-minion-id
                                         get-battlecries
                                         get-card
                                         get-card-buffs
                                         get-enemy-player
                                         get-enemy-player-id
                                         get-hand
                                         get-hero
                                         get-heroes
                                         get-last-minion-position
                                         get-mana
                                         get-max-mana
                                         get-minion
                                         get-minions
                                         get-minion-buffs
                                         get-player-id-in-turn
                                         get-players
                                         get-secrets
                                         get-times-hero-power-used
                                         inc-attacks-performed-minion
                                         minion-exists?
                                         next-minion-position
                                         poisonous?
                                         remove-card-from-hand
                                         remove-secret
                                         update-mana
                                         update-max-mana
                                         update-minion]]))


(defn change-player-turn
  "Changes the player whose turn it is."
  {:test (fn []
           (is= (-> (create-game)
                    (change-player-turn)
                    (get-player-id-in-turn))
                "p2")
           (is= (-> (create-game)
                    (change-player-turn)
                    (change-player-turn)
                    (get-player-id-in-turn))
                "p1"))}

  [state]
  (update state :player-id-in-turn (fn [player-in-turn]
                                     (:id (get-enemy-player state player-in-turn)))))

(defn increment-max-mana
  "Increments the maximum amount of mana that the given player can possess, if possible."
  {:test (fn []
           (is= (-> (create-game)
                    (increment-max-mana "p1")
                    (get-max-mana "p1"))
                10)
           (is= (-> (create-game)
                    (update-mana "p1" 5)
                    (update-max-mana "p1" 5)
                    (increment-max-mana "p1")
                    (get-max-mana "p1"))
                6))}

  [state player-id]

  (let [max-mana (get-max-mana state player-id)]
    (if (< max-mana 10)
      (update-max-mana state player-id inc)
      state)))

(defn restore-mana
  "Restores the given player's current mana back to their maximum."
  {:test (fn []
           (is= (-> (create-game)
                    (update-mana "p1" 5)
                    (restore-mana "p1")
                    (get-mana "p1"))
                10)
           (is= (-> (create-game)
                    (update-mana "p1" 2)
                    (update-max-mana "p1" 5)
                    (restore-mana "p1")
                    (get-mana "p1"))
                5))}

  [state player-id]
  (update-mana state player-id (get-max-mana state player-id)))

(defn do-start-turn-cycle
  "Increments the maximum mana of the player whose turn is beginning if possible, restores it to their maximum, and has them draw a card if possible."
  {:test (fn []
           (is= (-> (create-game)
                    (update-mana "p1" 2)
                    (update-max-mana "p1" 5)
                    (do-start-turn-cycle)
                    (get-mana "p1"))
                6)
           (is= (-> (create-game)
                    (update-mana "p1" 15)
                    (do-start-turn-cycle)
                    (get-mana "p1"))
                10)
           (is= (as-> (create-game) $
                      (do-start-turn-cycle $)
                      (:turn-index $))
                1)
           ;Action-index incremented
           (is= (as-> (create-game) $
                      (do-start-turn-cycle $)
                      (:action-index $))
                1))}

  [state]
  (let [new-player-id (get-player-id-in-turn state)]
    (as-> state $
          (update $ :turn-index inc)
          (update $ :action-index inc)
          (assoc-in $ [:players new-player-id :hero :times-hero-power-used] 0)
          (assoc-in $ [:minion-ids-summoned-this-turn] [])
          (reduce (fn [a m]
                    (update-minion a (:id m) :attacks-performed-this-turn 0))
                  $
                  (get-minions state new-player-id))

          (increment-max-mana $ new-player-id)
          (restore-mana $ new-player-id)
          (draw-card $ new-player-id))))

(defn end-turn
  "Changes the player turn and does the other beginning of turn actions regarding mana and drawing."
  {:test (fn []
           (is= (-> (create-game)
                    (end-turn)
                    (get-player-id-in-turn))
                "p2")
           (is= (-> (create-game)
                    (end-turn)
                    (get-mana "p2"))
                10)
           (is= (-> (create-game)
                    (update-mana "p2" 2)
                    (update-max-mana "p2" 5)
                    (end-turn)
                    (get-mana "p2"))
                6)
           (is= (-> (create-game [{:minions [(create-minion "Pippi") (create-minion "Emil")]}])
                    (end-turn)
                    (get-health "m3"))
                4)
           (is= (-> (create-game [{:minions [(create-minion "Pippi") (create-minion "Emil")]}])
                    (end-turn)
                    (get-health "m1"))
                4)
           (is= (-> (create-game)
                    (add-card-to-deck "p2" "Mio")
                    (end-turn)
                    (get-hand "p2")
                    (count))
                1))}

  [state]
  (-> state
      (trigger-action-on-minions :on-end-turn)
      (tick-down-temporary-buffs)
      (change-player-turn)
      (do-start-turn-cycle)))

(defn trigger-attack-secrets
  "Triggers secrets on attacking heroes or minions."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Vaporize")
                    (trigger-attack-secrets "p2" "e" "h1")
                    (get-minions "p2")
                    (count))
                0)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Vaporize")
                    (trigger-attack-secrets "p2" "e" "h1")
                    (get-secrets "p1")
                    (count))
                0)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Explosive Trap")
                    (trigger-attack-secrets "p2" "e" "h1")
                    (get-health "h2"))
                28)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Venomstrike Trap")
                    (trigger-attack-secrets "p2" "e" "m")
                    (get-minions "p1")
                    (count))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Explosive Trap")
                    (trigger-attack-secrets "p2" "e" "m")
                    (get-health "h2"))
                30)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Venomstrike Trap")
                    (trigger-attack-secrets "p2" "e" "h2")
                    (get-minions "p1")
                    (count))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Vaporize")
                    (trigger-attack-secrets "p2" "e" "m")
                    (get-minions "p2")
                    (count))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p1" "Vaporize")
                    (trigger-attack-secrets "p2" "e" "m")
                    (get-minions "p2")
                    (count))
                1))}

  [state player-id attacker-id target-id]
  (let [enemy-player-id (get-enemy-player-id state player-id)
        secrets (get-secrets state enemy-player-id)
        target-trigger (if (minion? state target-id)
                         :minion-attacked
                         :hero-attacked)]

    (reduce (fn [acc-state secret]
              (let [definition (get-definition secret)
                    secret-trigger (:secret-trigger definition)
                    secret-fn (:secret-fn definition)]
                (if (= secret-trigger target-trigger)
                  (-> acc-state
                      (secret-fn {:player-id enemy-player-id :attacker-id attacker-id :target-id target-id})
                      (remove-secret enemy-player-id (:name secret)))
                  acc-state)))
            state
            secrets)))

(defn attack
  "Allows the given player to launch an attack from the given attacker towards the given target."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m")]}])
                    (add-minion-to-board "p2" (create-minion "Emil" :owner-id "p2" :id "e") 0)
                    (attack "p1" "m" "e")
                    (get-health "e"))
                4)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :owner-id "p1" :id "e")]}])
                    (add-minion-to-board "p2" (create-minion "Kato" :owner-id "p2" :id "k") 0)
                    (attack "p1" "e" "k")
                    (get-health "e"))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m")]}])
                    (attack "p1" "m" "h2")
                    (get-health "h2"))
                29)
           (is-not (-> (create-game [{:minions [(create-minion "Herr Nilsson" :owner-id "p1" :id "hn")]}])
                       (add-minion-to-board "p2" (create-minion "Emil" :owner-id "p2" :id "e") 0)
                       (attack "p1" "hn" "e")
                       (minion-exists? "e")))
           (is-not (-> (create-game [{:minions [(create-minion "Herr Nilsson" :owner-id "p1" :id "hn")]}])
                       (add-minion-to-board "p2" (create-minion "Emil" :owner-id "p2" :id "e") 0)
                       (end-turn)
                       (attack "p2" "e" "hn")
                       (minion-exists? "e")))
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m") (create-minion "Emil" :owner-id "p1" :id "e")]}])
                    (attack "p1" "m" "h2")
                    (attack "p1" "e" "h2")
                    (get-health "h2"))
                27)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p2" "Vaporize")
                    (attack "p1" "m" "h2")
                    (get-minions "p1")
                    (count))
                0)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (add-secret "p2" "Explosive Trap")
                    (attack "p1" "m" "h2")
                    (get-health "h1"))
                28)
           (error? (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m")]}])
                       (attack "p1" "m" "h2")
                       (attack "p1" "m" "h2")))
           ;Action-index incremented
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m")]}]) $
                      (attack $ "p1" "m" "h2")
                      (:action-index $))
                1))}

  [state player-id attacker-id target-id]
  (when-not (valid-attack? state player-id attacker-id target-id)
    (error "Not a valid attack."))
  (let [attack-strength (if (poisonous? (get-minion state attacker-id))
                          (get-health state target-id)
                          (get-attack state attacker-id))]
    (as-> state $
          (update $ :action-index inc)
          (trigger-attack-secrets $ player-id attacker-id target-id)
          (if (minion-exists? $ attacker-id)
            (as-> $ %
                  (if (minion? % target-id)
                    (damage % attacker-id (if (poisonous? (get-minion state target-id))
                                            (get-health state attacker-id)
                                            (get-attack state target-id)))
                    %)
                  (damage % target-id attack-strength)
                  (inc-attacks-performed-minion % attacker-id))
            $)$)))

(defn battlecry
  "Does the battlecry of the given card and the parameter given and adds the battle cry to the list of battlecries used by the player.."
  {:test (fn []
           (is= (let [state (create-game)
                      kato-card (create-card "Kato")]
                  (-> state
                      (battlecry kato-card {:player-id "p1"})
                      (get-health "h2")))
                26)
           (is= (-> (battlecry (create-game) (create-card "Kato") {:player-id "p1"})
                    (battlecry (create-card "Emil") {:player-id "p1"})
                    (battlecry (create-card "Leeroy Jenkins") {:player-id "p1"})
                    (get-battlecries "p1"))
                [{:name "Kato", :counter 1} {:name "Emil", :counter 2} {:name "Leeroy Jenkins", :counter 3}]))}

  [state card {player-id :player-id target-id :target-id this-minion-id :this-minion-id}]
  (let [definition (get-definition card)
        battlecry (:battlecry definition)
        name (:name definition)]
    (if battlecry
      (-> state
          (add-battlecry-name player-id name)
          (battlecry {:player-id player-id :target-id target-id :this-minion-id this-minion-id}))
      state)))

(defn card-mana-check
  "Returns whether or not the given player has enough mana to cover the cost of the given card."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (card-mana-check "p1" "e"))
                true)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (update-mana "p1" 1)
                    (card-mana-check "p1" "e"))
                false))}

  [state player-id card-id]
  (>= (get-mana state player-id) (get-card-mana-cost state card-id)))

(defn power-mana-check
  "Returns whether or not the given player has enough mana to cover the cost of the given hero-power."
  {:test (fn []
           (is= (-> (create-game)
                    (power-mana-check "p1" "Blessing"))
                true)
           (is= (-> (create-game)
                    (update-mana "p1" 1)
                    (power-mana-check "p1" "Blessing"))
                false))}

  [state player-id power]
  (>= (get-mana state player-id) (:mana-cost (get-definition power))))

(defn take-card-mana
  "Subtracts the mana cost of the given card from the given player's mana."
  {:test (fn []
           (is= (-> (create-game)
                    (take-card-mana "p1" (create-card "Emil" :id "e"))
                    (get-mana "p1"))
                6)
           (is= (-> (create-game)
                    (take-card-mana "p1" (create-card "Mio" :id "m"))
                    (get-mana "p1"))
                9))}

  [state player-id card]
  (update-mana state player-id (fn [old-mana] (- old-mana (get-card-mana-cost card)))))

(defn take-hero-power-mana
  "Subtracts the mana cost of the given hero-power from the given player's mana."
  {:test (fn []
           (is= (-> (create-game)
                    (take-hero-power-mana "p1" (create-card "Emil" :id "e"))
                    (get-mana "p1"))
                6)
           (is= (-> (create-game)
                    (take-hero-power-mana "p1" (create-card "Mio" :id "m"))
                    (get-mana "p1"))
                9))}

  [state player-id hero-power]
  (update-mana state player-id (fn [old-mana] (- old-mana (:mana-cost (get-definition hero-power))))))

(defn transfer-buffs
  "Transfers buffs from a specified card to the specified minion."
  {:test (fn []
           ; A minion should appear at the board
           (is= (-> (create-game [{:hand    [(create-card "Emil" :id "e")]
                                   :minions [(create-minion "Mio" :id "m")]}])
                    (add-card-buff "e" {:attack 2 :health 2} :hand)
                    (transfer-buffs "e" "m")
                    (get-minion "m")
                    (:buffs))
                [{:attack 2 :health 2}]))}
  [state card-or-id minion-id]
  (add-minion-buffs state minion-id (get-card-buffs state card-or-id)))

(defn play-minion-card
  "Removes the given minion card from the given player's hand, places it on the board in the specified position, performs its battlecry, subtracts its mana cost from the player, and adds its id to the list of minions summoned this turn."
  {:test (fn []
           ; A minion should appear at the board
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (play-minion-card "p1" "e")
                    (get-minions "p1")
                    (first)
                    (:name))
                "Emil")
           ; The card should be erased from hand
           (is (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                   (play-minion-card "p1" "e")
                   (get-hand "p1")
                   (empty?)))
           ; Minion should be in next position
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e") (create-card "Mio" :id "m")]}])
                    (play-minion-card "p1" "e")
                    (play-minion-card "p1" "m")
                    (get-last-minion-position "p1"))
                1)
           ; Emil battlecry test
           (is= (-> (create-game [{:deck [(create-card "Mio" :id "m")]
                                   :hand [(create-card "Emil" :id "e")]}])
                    (play-minion-card "p1" "e")
                    (get-hand "p1")
                    (count))
                1)
           ; Kato Battlecry test
           (is= (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                    (play-minion-card "p1" "k")
                    (get-health "h2"))
                26)
           ; Can't play cards if max minions
           (error? (-> (create-game [{:hand [(create-card "Kato" :id "k")] :minions [(create-minion "Mio" :owner-id "p1" :id "m0") (create-minion "Mio" :owner-id "p1" :id "m1") (create-minion "Mio" :owner-id "p1" :id "m2") (create-minion "Mio" :owner-id "p1" :id "m3") (create-minion "Mio" :owner-id "p1" :id "m4") (create-minion "Mio" :owner-id "p1" :id "m5") (create-minion "Mio" :owner-id "p1" :id "m6")]}])
                       (play-minion-card "p1" "k")))
           ; Can't play cards with not enough
           (error? (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                       (update-mana "p1" 0)
                       (play-minion-card "p1" "k")))
           ; Playing card should decrease mana
           (is= (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                    (play-minion-card "p1" "k")
                    (get-mana "p1"))
                6)
           (is-not (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                       (play-minion-card "p1" "m")
                       (valid-attack? "p1" "m" "h2")))
           ; Minion should get buffs from its card
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (add-card-buff "m" {:attack 2 :health 2} :hand)
                    (play-minion-card "p1" "m")
                    (get-minion "m1")
                    (get-health))
                4)
           ; New minion should be on list of summoned this turn
           (is= (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                    (play-minion-card "p1" "k")
                    (:minion-ids-summoned-this-turn))
                ["m1"])
           ;Action-index incremented
           (is= (as-> (create-game [{:hand [(create-card "Mio" :id "m")]}]) $
                      (play-minion-card $ "p1" "m")
                      (:action-index $))
                1))}

  ([state player-id card-id target-id]
   (if (and (not (max-minions? state player-id)) (card-mana-check state player-id card-id))
     (let [card (get-card state card-id)
           [state new-minion-id] (generate-new-minion-id state)
           new-minion (create-minion (:name card) :id new-minion-id)]
       (-> state
           (update :action-index inc)
           (remove-card-from-hand player-id card-id)
           (add-minion-to-board player-id new-minion (next-minion-position state player-id))
           (transfer-buffs card new-minion-id)
           (battlecry card {:player-id player-id :target-id target-id :this-minion-id new-minion-id})
           (add-to-minion-ids-summoned-this-turn new-minion-id)
           (take-card-mana player-id card)))
     (error "Cannot play minion card.")))
  ([state player-id card-id]
   (play-minion-card state player-id card-id nil)))

(defn can-use-hero-power
  "Determines whether or not a hero power can be used."
  {:test (fn []
           ; Hero power is used
           (is (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                   (get-hero "h1")
                   (can-use-hero-power "Blessing")))
           (is (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                   (get-hero "h1")
                   (can-use-hero-power "Strengthen")))
           (is-not (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                       (update-times-hero-power-used "h1" inc)
                       (get-hero "h1")
                       (can-use-hero-power "Blessing"))))}

  [hero hero-power-or-name]
  (let [max-uses (:max-uses (get-definition hero-power-or-name))]
    (> max-uses (get-times-hero-power-used hero))))

(defn use-hero-power
  "Uses the specified hero power if the given player has enough mana."
  {:test (fn []
           ; Hero power is used
           (is (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                   (use-hero-power "p1" "e")
                   (get-minion "e")
                   (divine-shield?)))
           ; Hero power should only be able to be used once
           (error? (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                       (use-hero-power "p1" "e")
                       (damage "e" 2)
                       (use-hero-power "p1" "e")
                       (get-minion "e")
                       (divine-shield?)))
           ; Takes mana
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (use-hero-power "p1" "e")
                    (get-mana "p1"))
                8)
           ;Action-index incremented.
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                    (use-hero-power $ "p1" "e")
                    (:action-index $))
                1))}

  ([state player-id]
   (use-hero-power state player-id nil))
  ([state player-id target-id]
   (let [hero (get-hero state (get-hero-id-from-player state player-id))
         hero-power-name (:hero-power (get-definition hero))
         hero-power-fn (:power (get-definition hero-power-name))]
     (if (and (power-mana-check state player-id hero-power-name) (can-use-hero-power hero hero-power-name))
       (-> state
           (update :action-index inc)
           (hero-power-fn target-id)
           (take-hero-power-mana player-id hero-power-name)
           (update-times-hero-power-used (:id hero) inc))
       (error "Cannot use hero power.")))))

(defn play-spell-card
  "Plays a given spell card and does its effects."
  {:test (fn []
           ; The card should be erased from hand
           (is (-> (create-game [{:hand [(create-card "Insect Swarm" :id "i")]}])
                   (play-spell-card "p1" "i")
                   (get-hand "p1")
                   (empty?)))
           ; The spell should be implemented
           (is= (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")]}])
                    (play-spell-card "p1" "r" "h2")
                    (get-health "h2"))
                27)
           ; Can't play cards with not enough mana
           (error? (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")]}])
                       (update-mana "p1" 0)
                       (play-spell-card "p1" "r" "h2")))
           ; Playing card should decrease mana
           (is= (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")]}])
                    (play-spell-card "p1" "r" "h2")
                    (get-mana "p1"))
                8)
           (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")]}])
                    (play-spell-card "p1" "v")
                    (get-secrets "p1"))
                [{:name "Vaporize", :id "s1", :owner-id "p1", :entity-type :secret}])
           ;Action-index incremented
           (is= (as-> (create-game [{:hand [(create-card "Radar Raid" :id "r")]}]) $
                      (play-spell-card $ "p1" "r" "h2")
                      (:action-index $))
                1))}

  ([state player-id card-id]
   (play-spell-card state player-id card-id nil))
  ([state player-id card-id target-id]
   (if (card-mana-check state player-id card-id)
     (let [card (get-card state card-id)
           definition (get-definition card)
           effect-fn (:effect definition)]
       (as-> state $
             (update $ :action-index inc)
             (remove-card-from-hand $ player-id card-id)
             (take-card-mana $ player-id card)
             (if-not (secret? card)
               (effect-fn $ {:target-id target-id})
               (-> $
                   (add-secret player-id (:name card))
                   (trigger-action-on-minions :on-secret-play)))))
     (error "Cannot play spell card."))))

(defn get-valid-target-ids
  "Gets the ids of minions who can be targeted by some effect from the given minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-target-ids "Astrid"))
                [])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Uncle Nilsson" :id "un")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-target-ids "Astrid"))
                ["un"])
           (is= (-> (create-game)
                    (get-valid-target-ids "Annika"))
                [])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Uncle Nilsson" :id "un")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-target-ids "Annika"))
                ["m" "r" "un" "e"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-target-ids "Radar Raid"))
                ["m" "r" "k" "e" "h1" "h2"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-target-ids "Insect Swarm"))
                ["m" "r" "k" "e" "h1" "h2"]))}
  [state name]
  (let [definition (get-definition name)
        valid-target-fn (:valid-target-fn definition)]
    (reduce (fn [ids c]
              (if (valid-target-fn state {:target-id (:id c)})
                (conj ids (:id c))
                ids))
            []
            (get-characters state))))

(defn get-valid-minion-attack-ids
  "Gets the ids of minions who can be targeted by an attack from the given minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-minion-attack-ids "m"))
                ["k" "e" "h2"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Jonatan" :id "j")
                                             (create-minion "Emil" :id "e")]}])
                    (get-valid-minion-attack-ids "m"))
                ["j"])
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                               (create-minion "Ronja" :id "r")]}
                                    {:minions [(create-minion "Kato" :id "k")
                                               (create-minion "Emil" :id "e")]}]) $
                      (end-turn $)
                      (get-valid-minion-attack-ids $ (get-minion $ "k")))
                ["m" "r" "h1"])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Ronja" :id "r")]
                                   :hand    [(create-card "Elisabeth" :id "es")]}
                                  {:minions [(create-minion "Jonatan" :id "j")
                                             (create-minion "Emil" :id "e")]}])
                    (play-minion-card "p1" "es")
                    (get-valid-minion-attack-ids "es"))
                []))}

  ([state minion-or-id]
   (let [minion-id (if (string? minion-or-id)
                     minion-or-id
                     (:id minion-or-id))
         player-id (if (string? minion-or-id)
                     (:owner-id (get-minion state minion-or-id))
                     (:owner-id minion-or-id))]
     (reduce (fn [ids c]
               (if (valid-attack? state player-id minion-id (:id c))
                 (conj ids (:id c))
                 ids))
             []
             (get-characters state)))))

(defn playable-minion-card?
  "Determines whether or not the given minion-card is playable."
  {:test (fn []
           ; Can play normal minion card
           (is (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                   (playable-minion-card? "p1" "e")))
           ; Cannot play without enough mana
           (is-not (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                       (update-mana "p1" 0)
                       (playable-minion-card? "p1" "e")))
           ; Cannot play minion card if at max minions
           (is-not (-> (create-game [{:hand    [(create-card "Emil" :id "e")]
                                      :minions [(create-minion "Mio" :owner-id "p1" :id "m0") (create-minion "Mio" :owner-id "p1" :id "m1") (create-minion "Mio" :owner-id "p1" :id "m2") (create-minion "Mio" :owner-id "p1" :id "m3") (create-minion "Mio" :owner-id "p1" :id "m4") (create-minion "Mio" :owner-id "p1" :id "m5") (create-minion "Mio" :owner-id "p1" :id "m6")]}])
                       (playable-minion-card? "p1" "e"))))}

  [state player-or-id card-or-id]
  (let [player-id (if (string? player-or-id)
                    player-or-id
                    (:id player-or-id))
        card-id (if (string? card-or-id)
                  card-or-id
                  (:id card-or-id))]
    (and
      (not (max-minions? state player-id))
      (card-mana-check state player-id card-id))))

(defn playable-spell-card?
  "Determines whether or not the given spell-card is playable."
  {:test (fn []
           ; Can play normal spell cards
           (is (-> (create-game [{:hand [(create-card "Insect Swarm" :id "i")]}])
                   (playable-spell-card? "p1" "i")))
           ; Should not be able to play identical secret
           (is-not (-> (create-game [{:secrets ["Vaporize"]
                                      :hand    [(create-card "Vaporize" :id "v")]}])
                       (playable-spell-card? "p1" "v")))
           ; Cannot play without enough mana
           (is-not (-> (create-game [{:hand [(create-card "Insect Swarm" :id "i")]}])
                       (update-mana "p1" 0)
                       (playable-spell-card? "p1" "i"))))}
  [state player-or-id card-or-id]
  (let [player-id (if (string? player-or-id)
                    player-or-id
                    (:id player-or-id))
        card-id (if (string? card-or-id)
                  card-or-id
                  (:id card-or-id))
        card (get-card state card-id)]
    (and (card-mana-check state player-id card-id)
         (not (some (fn [s] (= (:name s) (:name (get-definition card))))
                    (get-secrets state player-id))))))
