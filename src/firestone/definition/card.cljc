(ns firestone.definition.card
  (:require [firestone.definitions :as definitions
             :refer [get-definition]]
            [firestone.construct :refer [add-card-buff
                                         add-minion-buff
                                         add-divine-shield
                                         add-minion-to-board
                                         add-secret
                                         add-secrets
                                         add-taunt
                                         character-exists?
                                         create-minion
                                         get-battlecries
                                         get-dead-minions
                                         get-deck
                                         get-enemy-hero
                                         get-enemy-minions
                                         get-enemy-player
                                         get-enemy-player-id
                                         get-enemy-secrets
                                         get-hand
                                         get-last-minion-position
                                         get-minion
                                         get-minions
                                         get-secrets
                                         has-ability?
                                         is-current-hero?
                                         minion-exists?
                                         minion-has-buff?
                                         next-minion-position
                                         remove-card-from-deck
                                         remove-minion
                                         remove-secret
                                         switch-secret-owner
                                         switch-owner]]
            [firestone.core-api :refer [battlecry]]
            [firestone.core :refer [damage
                                    damage-all-in-list
                                    draw-card
                                    get-character
                                    get-characters
                                    max-minions?
                                    minion?
                                    same-owner?
                                    secret?
                                    silence-minion
                                    summon]]
            [firestone.random :refer [get-random-according-to-fn
                                      get-random-battlecry-target
                                      get-random-enemy-minion-id
                                      get-n-random-enemy-minion-ids
                                      get-random-friendly-minion-id
                                      get-n-random-friendly-minion-ids
                                      get-random-minion-id
                                      get-n-random-minion-ids
                                      random]]))

(def card-definitions
  {

   "ZeroAttack"
   {:name      "ZeroAttack"
    :attack    0
    :health    2
    :mana-cost 1
    :type      :minion}

   "Mio"
   {:name      "Mio"
    :attack    1
    :health    2
    :mana-cost 1
    :type      :minion}

   "Ronja"
   {:name      "Ronja"
    :attack    3
    :health    2
    :mana-cost 2
    :type      :minion}

   "Kato"
   {:name        "Kato"
    :attack      4
    :health      1
    :mana-cost   4
    :type        :minion
    :description "Battlecry: Deal 4 damage to the enemy hero."
    :battlecry   (fn [state {player-id :player-id}]
                   (damage state (:id (get-enemy-hero state player-id)) 4))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :description "Battlecry: Draw a card."
    :battlecry   (fn [state {player-id :player-id}]
                   (draw-card state player-id))}

   "Jonatan"
   {:name        "Jonatan"
    :attack      3
    :health      6
    :mana-cost   4
    :type        :minion
    :set         :custom
    :description "Taunt."
    :abilities   [:taunt]}

   "Alfred"
   {:name          "Alfred"
    :attack        4
    :health        5
    :mana-cost     2
    :type          :minion
    :set           :custom
    :description   "Can't Attack."
    :cannot-attack true}

   "Uncle Melker"
   {:name        "Uncle Melker"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :set         :custom
    :description "Divine Shield."
    :abilities   [:divine-shield]}

   "Pippi"
   {:name        "Pippi"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :set         :custom
    :description "At the end of your turn deal 1 damage to all other minions."
    :on-end-turn (fn [state {minion-id :minion-id}]
                   (let [minions (remove (fn [x] (= (:id x) minion-id)) (get-minions state))]
                     (-> state
                         (damage-all-in-list minions 1))))}

   "Karlsson"
   {:name        "Karlsson"
    :attack      1
    :health      4
    :mana-cost   3
    :type        :minion
    :set         :custom
    :description "At the end of your turn give a random minion taunt."
    :on-end-turn (fn [state _]
                   (let [[state minion-id] (get-random-minion-id state)]
                     (add-taunt state minion-id)))}


   "Uncle Nilsson"
   {:name        "Uncle Nilsson"
    :attack      5
    :health      5
    :mana-cost   6
    :type        :minion
    :set         :custom
    :description "Deathrattle: Take control of a random enemy minion."
    :deathrattle (fn [state {player-id :player-id}]
                   (let [[state minion-id] (get-random-enemy-minion-id state player-id)]
                     (switch-owner state minion-id player-id)))}

   "Elisabeth"
   {:name        "Elisabeth"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :set         :custom
    :description "Taunt. Divine Shield."
    :abilities   [:divine-shield :taunt]}

   "Madicken"
   {:name        "Madicken"
    :attack      1
    :health      2
    :mana-cost   2
    :type        :minion
    :set         :custom
    :description "Deathrattle: Summon Elisabeth."
    :deathrattle (fn [state {player-id :player-id}]
                   (summon state player-id "Elisabeth"))}

   "Ida"
   {:name        "Ida"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :set         :custom
    :description "Whenever a minion takes damage, gain taunt."
    :on-damage   (fn [state {minion-id :minion-id}]
                   (add-taunt state minion-id))}

   "Insect Swarm"
   {:name            "Insect Swarm"
    :mana-cost       2
    :type            :spell
    :set             :custom
    :description     "Deal 2 damage to all characters."
    :valid-target-fn (fn [_ _]
                       true)
    :effect          (fn [state _]
                       (let [characters (get-characters state)]
                         (damage-all-in-list state characters 2)))}

   "Radar Raid"
   {:name            "Radar Raid"
    :mana-cost       2
    :type            :spell
    :set             :custom
    :description     "Deal 3 damage to a character."
    :needs-targets   true
    :valid-target-fn (fn [state {target-id :target-id}]
                       (character-exists? state target-id))
    :effect          (fn [state {target-id :target-id}]
                       (damage state target-id 3))}

   "Herr Nilsson"
   {:name        "Herr Nilsson"
    :attack      1
    :health      3
    :mana-cost   3
    :type        :minion
    :set         :custom
    :description "Poisonous."
    :abilities   [:poisonous]}

   "Rasmus"
   {:name        "Rasmus"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :set         :custom
    :description "Windfury."
    :abilities   [:windfury]}

   "Tjorven"
   {:name        "Tjorven"
    :attack      0
    :health      2
    :mana-cost   5
    :type        :minion
    :set         :custom
    :description "Your other minions have windfury."
    :aura        (fn [_ this character]
                   (when (and (same-owner? this character)
                              (minion? character))
                     {:windfury true}))}

   "Astrid"
   {:name            "Astrid"
    :attack          3
    :health          3
    :mana-cost       4
    :type            :minion
    :set             :custom
    :description     "Battlecry: Copy another minion's deathrattle."
    :battlecry       (fn [state {target-id :target-id this-minion-id :this-minion-id}]
                       (if target-id
                         (let [target-minion-name (:name (get-definition (get-minion state target-id)))]
                           (add-minion-buff state this-minion-id {:deathrattle target-minion-name}))
                         state))
    :needs-targets   true
    :target-type     :any-minion
    :valid-target-fn (fn [state {target-id :target-id}]
                       (minion-has-buff? state target-id :deathrattle))}

   "Skrallan"
   {:name                  "Skrallan"
    :attack                2
    :health                2
    :mana-cost             3
    :type                  :minion
    :set                   :custom
    :description           "After a friendly minion loses Divine Shield, gain +2/+2."
    :on-divine-shield-loss (fn [state {minion-id :minion-id target-id :target-id}]
                             (if (same-owner? state minion-id target-id)
                               (add-minion-buff state minion-id {:attack 2 :health 2})
                               state))}

   "Annika"
   {:name            "Annika"
    :attack          2
    :health          2
    :mana-cost       3
    :type            :minion
    :set             :custom
    :description     "Battlecry: Give a minion +2 Attack this turn."
    :battlecry       (fn [state {target-id :target-id}]
                       (if target-id
                         (add-minion-buff state target-id {:attack 2 :duration 1})
                         state))
    :needs-targets   true
    :target-type     :any-minion
    :valid-target-fn (fn [state {target-id :target-id}]
                       (minion? state target-id))}

   "Al'Akir the Windlord"
   {:name        "Al'Akir the Windlord"
    :attack      3
    :health      5
    :mana-cost   8
    :type        :minion
    :set         :classic
    :rarity      :legendary
    :description "Windfury, Charge, Divine Shield, Taunt"
    :abilities   [:windfury :charge :divine-shield :taunt]}

   "Secretkeeper"
   {:name           "Secretkeeper"
    :attack         1
    :health         2
    :mana-cost      1
    :type           :minion
    :set            :classic
    :rarity         :rare
    :description    "Whenever a Secret is played, gain +1/+1."
    :on-secret-play (fn [state {minion-id :minion-id}]
                      (add-minion-buff state minion-id {:attack 1 :health 1}))}

   "Mad Scientist"
   {:name        "Mad Scientist"
    :attack      2
    :health      2
    :mana-cost   2
    :type        :minion
    :set         :curse-of-naxxramas
    :rarity      :common
    :description "Deathrattle: Put a Secret from your deck into the battlefield."
    :deathrattle (fn [state {player-id :player-id}]
                   (let [deck (get-deck state player-id)
                         [new-state selected-secret-card] (get-random-according-to-fn state deck
                                                                                      (fn [card]
                                                                                        (secret? card)))]
                     (as-> new-state $
                           (add-secret $ player-id (:name selected-secret-card))
                           (remove-card-from-deck $ player-id (:id selected-secret-card)))))}

   "Eater of Secrets"
   {:name        "Eater of Secrets"
    :attack      2
    :health      4
    :mana-cost   4
    :type        :minion
    :set         :whispers-of-the-old-gods
    :rarity      :rare
    :description "Battlecry: Destroy all enemy Secrets. Gain +1/+1 for each."
    :battlecry   (fn [state {player-id :player-id this-minion-id :this-minion-id}]
                   (let [enemy-player-id (:id (get-enemy-player state player-id))]
                     (reduce (fn [acc-state secret]
                               (-> acc-state
                                   (remove-secret enemy-player-id (:name secret))
                                   (add-minion-buff this-minion-id {:attack 1 :health 1})))
                             state
                             (get-secrets state enemy-player-id))))}

   "Kezan Mystic"
   {:name        "Kezan Mystic"
    :attack      4
    :health      3
    :mana-cost   4
    :type        :minion
    :set         :goblins-vs-gnomes
    :rarity      :rare
    :description "Battlecry: Take control of a random enemy Secret."
    :battlecry   (fn [state {player-id :player-id}]
                   (if (< 0 (count (get-enemy-secrets state player-id)))
                     (let [[new-state random-enemy-secret] (random state (get-enemy-secrets state player-id))]
                       (switch-secret-owner new-state
                                            (:name random-enemy-secret)
                                            player-id))
                     state))}

   "Stormwind Knight"
   {:name        "Stormwind Knight"
    :attack      2
    :health      5
    :mana-cost   4
    :set         :basic
    :type        :minion
    :description "Charge"
    :abilities   [:charge]}

   "Leeroy Jenkins"
   {:name        "Leeroy Jenkins"
    :attack      6
    :health      2
    :mana-cost   5
    :type        :minion
    :set         :classic
    :rarity      :legendary
    :description "Charge. Battlecry: Summon two 1/1 Whelps for your opponent."
    :abilities   [:charge]
    :battlecry   (fn [state {player-id :player-id}]
                   (-> state
                       (summon (:id (get-enemy-player state player-id)) "Whelp")
                       (summon (:id (get-enemy-player state player-id)) "Whelp")))}

   "The Mistcaller"
   {:name        "The Mistcaller"
    :attack      4
    :health      4
    :mana-cost   6
    :type        :minion
    :set         :the-grand-tournament
    :rarity      :legendary
    :description "Battlecry: Give all minions in your hand and deck +1/+1."
    :battlecry   (fn [state {player-id :player-id}]
                   (let [hand (get-hand state player-id)
                         deck (get-deck state player-id)]
                     (as-> state $
                           (reduce (fn [acc-state c]
                                     (add-card-buff acc-state (:id c) {:attack 1 :health 1} :hand))
                                   $
                                   hand)
                           (reduce (fn [acc-state c]
                                     (add-card-buff acc-state (:id c) {:attack 1 :health 1} :deck))
                                   $
                                   deck))))}
   "Spellbreaker"
   {:name            "Spellbreaker"
    :attack          4
    :health          3
    :mana-cost       4
    :type            :minion
    :set             :classic
    :rarity          :common
    :description     "Battlecry: Silence a minion."
    :battlecry       (fn [state {target-id :target-id}]
                       (if target-id
                         (silence-minion state target-id)
                         state))
    :needs-targets   true
    :target-type     :any-minion
    :valid-target-fn (fn [state {target-id :target-id}]
                       (minion? state target-id))}

   "Shudderwock"
   {:name        "Shudderwock"
    :attack      6
    :health      6
    :mana-cost   9
    :type        :minion
    :set         :the-witchwood
    :rarity      :legendary
    :description "Battlecry: Repeat all other Battlecries from cards you played this game (targets chosen randomly)."
    :battlecry   (fn [state {player-id :player-id this-minion-id :this-minion-id}]
                   (reduce (fn [acc-state b]
                             (if-not (= (:name b) "Shudderwock")
                               (let [definition (get-definition b)
                                     battlecry-fn (:battlecry definition)
                                     target-id (:id (get-random-battlecry-target acc-state player-id (:name b)))]
                                 (battlecry-fn acc-state {:player-id player-id :target-id target-id :this-minion-id this-minion-id}))
                               acc-state))
                           state
                           (sort-by :counter (get-battlecries state player-id))))}

   "Silence"
   {:name            "Silence"
    :mana-cost       0
    :type            :spell
    :set             :classic
    :rarity          :common
    :description     "Silence a minion."
    :valid-target-fn (fn [state {target-id :target-id}]
                       (minion? state target-id))
    :needs-targets   true
    :effect          (fn [state {target-id :target-id}]
                       (silence-minion state target-id))}

   "Explosive Trap"
   {:name            "Explosive Trap"
    :mana-cost       2
    :type            :spell
    :sub-type        :secret
    :set             :classic
    :rarity          :common
    :description     "Secret: When your hero is attacked deal 2 damage to all enemies."
    :valid-target-fn (fn [state {target-id :target-id}]
                       (is-current-hero? state target-id))
    :secret-trigger  :hero-attacked
    :secret-fn       (fn [state {player-id :player-id}]
                       (let [enemy-characters (conj (get-enemy-minions state player-id)
                                                    (get-enemy-hero state player-id))]
                         (damage-all-in-list state enemy-characters 2)))}

   "Venomstrike Trap"
   {:name            "Venomstrike Trap"
    :mana-cost       2
    :type            :spell
    :sub-type        :secret
    :set             :knights-of-the-frozen-throne
    :rarity          :rare
    :description     "Secret: When one of your minions is attacked summon a 2/3 Poisonous Cobra."
    :valid-target-fn (fn [state {target-id :target-id}]
                       (is-current-hero? state target-id))
    :secret-trigger  :minion-attacked
    :secret-fn       (fn [state {player-id :player-id}]
                       (summon state player-id "Emperor Cobra"))}

   "Vaporize"
   {:name            "Vaporize"
    :mana-cost       3
    :type            :spell
    :sub-type        :secret
    :set             :classic
    :rarity          :rare
    :description     "Secret: When a minion attacks your hero destroy it."
    :valid-target-fn (fn [state {target-id :target-id}]
                       (is-current-hero? state target-id))
    :secret-trigger  :hero-attacked
    :secret-fn       (fn [state {attacker-id :attacker-id}]
                       (remove-minion state attacker-id))}

   "Whelp"
   {:name      "Whelp"
    :attack    1
    :health    1
    :mana-cost 1
    :set       :classic
    :type      :minion
    :rarity    :common}

   "Emperor Cobra"
   {:name        "Emperor Cobra"
    :attack      2
    :health      3
    :mana-cost   3
    :type        :minion
    :set         :classic
    :rarity      :rare
    :description "Poisonous."
    :abilities   [:poisonous]}

   ;; Sprint 5 definitions

   "Acolyte of Pain"
   {:name        "Acolyte of Pain"
    :attack      1
    :health      3
    :mana-cost   3
    :type        :minion
    :rarity      :common
    :set         :classic
    :description "Whenever this minion takes damage, draw a card."
    :on-damage   (fn [state {my-id :minion-id}]
                   (let [player-id (:owner-id (get-minion state my-id))]
                     (draw-card state player-id)))}

   "Flesheating Ghoul"
   {:name             "Flesheating Ghoul"
    :attack           2
    :health           3
    :mana-cost        3
    :set              :classic
    :rarity           :common
    :type             :minion
    :description      "Whenever a minion dies, gain +1 Attack."
    :when-minion-dies (fn [state {my-id :minion-id}]
                        (add-minion-buff state my-id {:attack 1}))}

   "Hadronox"
   {:name        "Hadronox"
    :attack      3
    :health      7
    :mana-cost   9
    :type        :minion
    :set         :knights-of-the-frozen-throne
    :rarity      :legendary
    :description "Deathrattle: Summon your Taunt minions that died this game."
    :deathrattle (fn [state {player-id :player-id}]
                   (reduce (fn [acc-state minion]
                             (if (has-ability? acc-state minion :taunt)
                               (summon acc-state (:owner-id minion) (:name minion))
                               acc-state))
                           state
                           (get-dead-minions state player-id)))
    }


   "Knife Juggler"
   {:name        "Knife Juggler"
    :attack      2
    :health      2
    :mana-cost   2
    :type        :minion
    :set         :classic
    :rarity      :rare
    :description "After you summon a minion, deal 1 damage to a random enemy."
    :on-summon   (fn [state {my-id :minion-id}]
                   (let [[state minion-id] (get-random-enemy-minion-id state (:owner-id (get-minion state my-id)))]
                     (if (minion-exists? state minion-id)
                       (damage state minion-id 1)
                       state)
                     ))
    }

   "Snake"
   {:name      "Snake"
    :attack    1
    :health    1
    :mana-cost 1
    :type      :minion
    :set       :classic}

   "Snake Trap"
   {:name           "Snake Trap"
    :type           :spell
    :mana-cost      2
    :set            :classic
    :rarity         :epic
    :sub-type       :secret
    :description    "Secret: When one of your minions is attacked summon three 1/1 Snakes."
    :secret-trigger :minion-attacked
    :secret-fn      (fn [state {player-id :player-id}]
                      (-> state
                          (summon player-id "Snake")
                          (summon player-id "Snake")
                          (summon player-id "Snake")))}

   })

(definitions/add-definitions! card-definitions)

