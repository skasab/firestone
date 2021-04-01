(ns firestone.construct
  (:require [ysera.test :refer [is is-not is= error?]]
            [firestone.definitions :refer [get-definition]]))

(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Carl")
                {:name                  "Carl"
                 :entity-type           :hero
                 :damage-taken          0
                 :times-hero-power-used 0})
           (is= (create-hero "Carl" :damage-taken 10)
                {:name                  "Carl"
                 :entity-type           :hero
                 :damage-taken          10
                 :times-hero-power-used 0}))}
  [name & kvs]
  (let [hero {:name                  name
              :entity-type           :hero
              :damage-taken          0
              :times-hero-power-used 0}]
    (if (empty? kvs)
      hero
      (apply assoc hero kvs))))

(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-card "Mio" :id "m")
                {:id          "m"
                 :entity-type :card
                 :name        "Mio"
                 :buffs       []}))}
  [name & kvs]
  (let [card {:name        name
              :entity-type :card
              :buffs       []}]
    (if (empty? kvs)
      card
      (apply assoc card kvs))))

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Mio" :id "m" :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :entity-type                 :minion
                 :name                        "Mio"
                 :id                          "m"
                 :abilities                   []
                 :buffs                       []})
           (is= (create-minion "Uncle Melker")
                {:damage-taken                0
                 :entity-type                 :minion
                 :name                        "Uncle Melker"
                 :attacks-performed-this-turn 0
                 :abilities                   [:divine-shield]
                 :buffs                       []})
           (is= (-> (create-minion "Uncle Nilsson")
                    (:buffs))
                [{:deathrattle "Uncle Nilsson"}])
           (is= (-> (create-minion "Pippi")
                    (:buffs))
                [{:on-end-turn "Pippi"}]))}
  [name & kvs]
  (let [definition (get-definition name)                    ; Will be used later
        minion {:damage-taken                0
                :entity-type                 :minion
                :name                        name
                :attacks-performed-this-turn 0
                :abilities                   (or (:abilities definition)
                                                 [])
                :buffs                       (reduce (fn [buffs buff-name]
                                                       (if (contains? definition buff-name)
                                                         (conj buffs {buff-name name})
                                                         buffs))
                                                     []
                                                     [:deathrattle :aura :on-end-turn :on-divine-shield-loss :on-damage :on-secret-play :when-minion-dies :on-summon])
                }]
    (if (empty? kvs)
      minion
      (apply assoc minion kvs))))

(defn create-empty-state
  "Creates an empty state with the given heroes."
  {:test (fn []
           (is= (create-empty-state [(create-hero "Carl")
                                     (create-hero "Carl")])
                (create-empty-state))

           (is= (create-empty-state [(create-hero "Carl" :id "c")
                                     (create-hero "Gustaf")])
                {:player-id-in-turn             "p1"
                 :players                       {"p1" {:id           "p1"
                                                       :mana         10
                                                       :max-mana     10
                                                       :fatigue      0
                                                       :battlecries  []
                                                       :secrets      []
                                                       :deck         []
                                                       :hand         []
                                                       :minions      []
                                                       :dead-minions []
                                                       :hero         {:name                  "Carl"
                                                                      :id                    "c"
                                                                      :owner-id              "p1"
                                                                      :damage-taken          0
                                                                      :entity-type           :hero
                                                                      :times-hero-power-used 0}}
                                                 "p2" {:id           "p2"
                                                       :mana         10
                                                       :max-mana     10
                                                       :fatigue      0
                                                       :battlecries  []
                                                       :secrets      []
                                                       :deck         []
                                                       :hand         []
                                                       :minions      []
                                                       :dead-minions []
                                                       :hero         {:name                  "Gustaf"
                                                                      :id                    "h2"
                                                                      :owner-id              "p2"
                                                                      :damage-taken          0
                                                                      :entity-type           :hero
                                                                      :times-hero-power-used 0}}}
                 :counter                       1
                 :seed                          0
                 :minion-ids-summoned-this-turn []
                 :turn-index                    0
                 :action-index                  0}))}
  ([heroes]
   ; Creates Carl heroes if heroes are missing.
   (let [heroes (->> (concat heroes [(create-hero "Carl")
                                     (create-hero "Carl")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id           (str "p" (inc index))
                                                          :mana         10
                                                          :max-mana     10
                                                          :fatigue      0
                                                          :battlecries  []
                                                          :secrets      []
                                                          :deck         []
                                                          :hand         []
                                                          :minions      []
                                                          :dead-minions []
                                                          :hero         (-> (if (contains? hero :id)
                                                                              hero
                                                                              (assoc hero :id (str "h" (inc index))))
                                                                            (assoc :owner-id (str "p" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :seed                          0
      :minion-ids-summoned-this-turn []
      :turn-index                    0
      :action-index                  0}))
  ([]
   (create-empty-state [])))

(defn get-player
  "Returns the player with the given id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player "p1")
                    (:id))
                "p1"))}
  [state player-id]
  (get-in state [:players player-id]))

(defn get-player-id-in-turn
  "Returns the player whose turn it is."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player-id-in-turn))
                "p1"))}
  [state]
  (:player-id-in-turn state))

(defn get-minions
  "Returns the minions on the board for the given player-id or for both players."
  {:test (fn []
           ; Getting minions is also tested in add-minion-to-board.
           (is= (-> (create-empty-state)
                    (get-minions "p1"))
                [])
           (is= (-> (create-empty-state)
                    (get-minions))
                []))}
  ([state player-id]
   (:minions (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :minions)
        (apply concat))))

(defn get-deck
  "Returns the deck for the given player-id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-deck "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :deck]))

(defn get-hand
  "Returns the hand of the given player-id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-hand "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :hand]))

(defn- generate-id
  "Generates an id and returns a tuple with the new state and the generated id."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(defn- generate-time-id
  "Generates a number and returns a tuple with the new state and the generated number."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(defn generate-new-minion-id
  "Generates a new minion id."
  {:test (fn []
           (is= (generate-new-minion-id {:counter 6})
                [{:counter 7} "m6"]))}
  [state]
  (let [[state value] (generate-id state)]
    [state (str "m" value)])
  )

(defn add-minion-to-board
  "Adds a minion with a given position to a player's minions and updates the other minions' positions."
  {:test (fn []
           ; Adding a minion to an empty board
           (is= (as-> (create-empty-state) $
                      (add-minion-to-board $ "p1" (create-minion "Mio" :id "m") 0)
                      (get-minions $ "p1")
                      (map (fn [m] {:id (:id m) :name (:name m)}) $))
                [{:id "m" :name "Mio"}])
           ; Adding a minion and update positions
           (let [minions (-> (create-empty-state)
                             (add-minion-to-board "p1" (create-minion "Mio" :id "m1") 0)
                             (add-minion-to-board "p1" (create-minion "Mio" :id "m2") 0)
                             (add-minion-to-board "p1" (create-minion "Mio" :id "m3") 1)
                             (get-minions "p1"))]
             (is= (map :id minions) ["m1" "m2" "m3"])
             (is= (map :position minions) [2 0 1]))
           ; Generating an id for the new minion
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Mio") 0))]
             (is= (-> (get-minions state "p1")
                      (first)
                      (:id))
                  "m1")
             (is= (:counter state) 3)))}
  [state player-id minion position]
  {:pre [(map? state) (string? player-id) (= (:entity-type minion) :minion) (number? position)]}
  (let [[state id] (if (contains? minion :id)
                     [state (:id minion)]
                     (generate-new-minion-id state))
        [state time-id] (generate-time-id state)
        ready-minion (assoc minion :position position
                                   :owner-id player-id
                                   :id id
                                   :added-to-board-time-id time-id)]
    (update-in state
               [:players player-id :minions]
               (fn [minions]
                 (conj (->> minions
                            (mapv (fn [m]
                                    (if (< (:position m) position)
                                      m
                                      (update m :position inc)))))
                       ready-minion)))))

(defn add-minions-to-board
  "Adds the given minions to the board on the side of the given player."
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-minions-to-board $ "p1" [(create-minion "Mio")
                                                    (create-minion "Ronja")
                                                    (create-minion "Kato")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Mio" "Ronja" "Kato"]))}
  [state player-id minions]
  (->> minions
       (reduce-kv (fn [state index minion]
                    (add-minion-to-board state
                                         player-id
                                         (if (string? minion)
                                           (create-minion minion)
                                           minion)
                                         index))
                  state)))

(defn- add-card-to
  "Adds a card to either the hand or the deck."
  {:test (fn []
           ; Adding cards to deck
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Mio" :deck)
                      (add-card-to $ "p1" "Kato" :deck)
                      (get-deck $ "p1")
                      (map :name $))
                ["Mio" "Kato"])
           ; Adding cards to hand
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Mio" :hand)
                      (add-card-to $ "p1" "Kato" :hand)
                      (get-hand $ "p1")
                      (map :name $))
                ["Mio" "Kato"]))}
  [state player-id card-or-name place]
  (let [card (if (string? card-or-name)
               (create-card card-or-name)
               card-or-name)
        [state id] (if (contains? card :id)
                     [state (:id card)]
                     (let [[state value] (generate-id state)]
                       [state (str "c" value)]))
        ready-card (assoc card :owner-id player-id
                               :id id)]
    (update-in state [:players player-id place] conj ready-card)))

(defn add-card-to-deck
  "Adds the given card to the given player's deck."
  [state player-id card]
  (add-card-to state player-id card :deck))

(defn add-card-to-hand
  "Adds the given card to the given player's hand."
  [state player-id card]
  (add-card-to state player-id card :hand))

(defn add-cards-to-deck
  "Adds the given cards to the given player's deck."
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-deck state player-id card))
          state
          cards))

(defn add-cards-to-hand
  "Adds the given cards to the given player's hand."
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-hand state player-id card))
          state
          cards))

(defn get-mana
  "Returns the amount of mana the given player has."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-mana "p1"))
                10))}
  [state player-id]
  (get-in state [:players player-id :mana]))

(defn update-mana
  "Updates the amount of mana that the given player has."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (update-mana "p1" 4)
                    (get-mana "p1"))
                4)
           (is= (-> (create-empty-state)
                    (update-mana "p1" dec)
                    (get-mana "p1"))
                9))}
  [state player-id fn-or-value]
  (if (fn? fn-or-value)
    (update-in state [:players player-id :mana] fn-or-value)
    (assoc-in state [:players player-id :mana] fn-or-value)))

(defn get-max-mana
  "Returns the maximum amount of mana that the given player can have."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-max-mana "p1"))
                10))}
  [state player-id]
  (get-in state [:players player-id :max-mana]))

(defn update-max-mana
  "Updates the maximum amount of mana that the given player can have."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (update-max-mana "p1" 4)
                    (get-max-mana "p1"))
                4)
           (is= (-> (create-empty-state)
                    (update-max-mana "p1" dec)
                    (get-max-mana "p1"))
                9))}
  [state player-id fn-or-value]
  (if (fn? fn-or-value)
    (update-in state [:players player-id :max-mana] fn-or-value)
    (assoc-in state [:players player-id :max-mana] fn-or-value)))

(defn get-battlecries
  "Returns the list of names in the player's battlecry list."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-battlecries "p1"))
                [])
           (is= (-> (create-empty-state)
                    (assoc-in [:players "p1" :battlecries] ["Astrid" "Emil"])
                    (get-battlecries "p1"))
                ["Astrid" "Emil"])
           (is= (-> (create-empty-state)
                    (assoc-in [:players "p1" :battlecries] ["Astrid" "Emil"])
                    (update-in [:players "p1" :battlecries] (fn [old-cries] (conj old-cries "Annika")))
                    (get-battlecries "p1"))
                ["Astrid" "Emil" "Annika"]))}
  [state player-id]
  (get-in state [:players player-id :battlecries]))

(defn add-battlecry-name
  "Adds the given name to the list of battlecries of the player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (add-battlecry-name "p1" "Emil")
                    (get-battlecries "p1"))
                [{:name "Emil", :counter 1}]))}
  [state player-id battlecry-name]
  (let [[new-state value] (generate-id state)
        ready-battlecry {:name    battlecry-name
                         :counter value}]
    (update-in new-state [:players player-id :battlecries]
               (fn [battlecries] (conj battlecries ready-battlecry)))))

(defn get-secrets
  "Returns the secrets of the given player-id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-secrets "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :secrets]))

(defn get-secret
  "Returns the secret of the given player-id with the given name"
  [state player-id secret-name]
  (->> (get-secrets state player-id)
       (filter (fn [s] (= (:name s) secret-name)))
       (first)))

(defn add-secret
  "Adds the given secret to the given player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (add-secret "p1" "Explosive Trap")
                    (get-secrets "p1"))
                [{:name "Explosive Trap", :id "s1", :owner-id "p1", :entity-type :secret}]))}
  [state player-id secret]
  (let [[new-state value] (generate-id state)
        new-secret {:name        secret
                    :id          (str "s" value)
                    :owner-id    player-id
                    :entity-type :secret}]
    (update-in new-state [:players player-id :secrets]
               (fn [secrets] (conj secrets new-secret)))))

(defn add-secrets
  "Adds the given secrets to the given player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (add-secrets "p1" ["Explosive Trap" "Venomstrike Trap"])
                    (get-secrets "p1"))
                [{:name "Explosive Trap", :id "s1", :owner-id "p1", :entity-type :secret}
                 {:name "Venomstrike Trap", :id "s2", :owner-id "p1", :entity-type :secret}]))}
  [state player-id secrets]
  (reduce (fn [a-state secret-name]
            (add-secret a-state player-id secret-name))
          state
          secrets))

(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))

           (is= (create-game [{:hero (create-hero "Carl")}])
                (create-game [{:hero "Carl"}]))

           (is= (create-game [{:minions [(create-minion "Mio")]}])
                (create-game [{:minions ["Mio"]}]))

           (is= (create-game [{:minions ["Mio"]
                               :deck    ["Ronja"]
                               :hand    ["Emil"]
                               :mana    3}
                              {:hero "Carl"}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id           "p1"
                                                       :mana         3
                                                       :max-mana     3
                                                       :fatigue      0
                                                       :battlecries  []
                                                       :secrets      []
                                                       :deck         [{:entity-type :card
                                                                       :id          "c3"
                                                                       :name        "Ronja"
                                                                       :owner-id    "p1"
                                                                       :buffs       []}]
                                                       :hand         [{:entity-type :card
                                                                       :id          "c4"
                                                                       :name        "Emil"
                                                                       :owner-id    "p1"
                                                                       :buffs       []}]
                                                       :minions      [{:damage-taken                0
                                                                       :attacks-performed-this-turn 0
                                                                       :added-to-board-time-id      2
                                                                       :entity-type                 :minion
                                                                       :name                        "Mio"
                                                                       :id                          "m1"
                                                                       :position                    0
                                                                       :owner-id                    "p1"
                                                                       :abilities                   []
                                                                       :buffs                       []}]
                                                       :dead-minions []
                                                       :hero         {:name                  "Carl"
                                                                      :id                    "h1"
                                                                      :owner-id              "p1"
                                                                      :entity-type           :hero
                                                                      :damage-taken          0
                                                                      :times-hero-power-used 0}}
                                                 "p2" {:id           "p2"
                                                       :mana         10
                                                       :max-mana     10
                                                       :fatigue      0
                                                       :battlecries  []
                                                       :secrets      []
                                                       :deck         []
                                                       :hand         []
                                                       :minions      []
                                                       :dead-minions []
                                                       :hero         {:name                  "Carl"
                                                                      :id                    "h2"
                                                                      :owner-id              "p2"
                                                                      :entity-type           :hero
                                                                      :damage-taken          0
                                                                      :times-hero-power-used 0}}}
                 :counter                       5
                 :seed                          0
                 :minion-ids-summoned-this-turn []
                 :turn-index                    0
                 :action-index                  0}))}
  ([data & kvs]
   (let [players-data (map-indexed (fn [index player-data]
                                     (assoc player-data :player-id (str "p" (inc index))))
                                   data)
         state (as-> (create-empty-state (map (fn [player-data]
                                                (cond (nil? (:hero player-data))
                                                      (create-hero "Carl")

                                                      (string? (:hero player-data))
                                                      (create-hero (:hero player-data))

                                                      :else
                                                      (:hero player-data)))
                                              data)) $
                     (reduce (fn [state {player-id :player-id
                                         mana      :mana
                                         minions   :minions
                                         deck      :deck
                                         hand      :hand
                                         secrets   :secrets}]
                               (-> (if mana
                                     (-> state
                                         (update-mana player-id mana)
                                         (update-max-mana player-id mana))
                                     state)
                                   (add-minions-to-board player-id minions)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)
                                   (add-secrets player-id secrets)))
                             $
                             players-data))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))

(defn get-minion
  "Returns the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-minion "m")
                    (:name))
                "Mio"))}
  [state id]
  (->> (get-minions state)
       (filter (fn [m] (= (:id m) id)))
       (first)))

(defn get-players
  "Returns the players of the game."
  {:test (fn []
           (is= (->> (create-game)
                     (get-players)
                     (map :id))
                ["p1" "p2"]))}
  [state]
  (->> (:players state)
       (vals)))

(defn get-enemy-player
  "Returns the player who is the opponent of the given player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-enemy-player "p1")
                    (:id))
                "p2"))}
  [state player-id]
  (->> (get-players state)
       (filter (fn [p] (not= (:id p) player-id)))
       (first)))

(defn get-enemy-player-id
  "Returns the id of the opponent of the given player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-enemy-player-id "p1"))
                "p2"))}
  [state player-id]
  (:id (get-enemy-player state player-id)))

(defn get-enemy-hero
  "Returns the hero of the opponent of the given player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-enemy-hero "p1")
                    (:id))
                "h2"))}
  [state player-id]
  (:hero (get-enemy-player state player-id)))


(defn get-enemy-minions
  "Returns the minions of the opponent of the given player."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (get-enemy-minions "p2")
                    (count))
                1))}
  [state player-id]
  (get-minions state (:id (get-enemy-player state player-id))))


(defn get-heroes
  "Returns the heroes in the game."
  {:test (fn []
           (is= (->> (create-game)
                     (get-heroes)
                     (map :name))
                ["Carl" "Carl"]))}
  [state]
  (->> (get-players state)
       (map :hero)))

(defn get-hero
  "Returns the hero specified by the hero-id."
  {:test (fn []
           (is= (-> (create-game)
                    (get-hero, "h1")
                    (:name,))
                "Carl"))}
  [state hero-id]
  (->> (get-heroes state)
       (filter (fn [h] (= (:id h) hero-id)))
       (first)))

(defn get-hero-by-player-id
  "Returns the hero of the player with the given id."
  {:test (fn []
           (is= (-> (create-game)
                    (get-hero-by-player-id "p1")
                    (:id))
                "h1"))}
  [state player-id]
  (get-in state [:players player-id :hero]))

(defn get-enemy-hero-by-player-id
  "Returns the enemy hero of the player with the given id."
  {:test (fn []
           (is= (-> (create-game)
                    (get-enemy-hero-by-player-id "p1")
                    (:id))
                "h2"))}
  [state player-id]
  (let [enemy-player-id (get-enemy-player-id state player-id)]
    (get-in state [:players enemy-player-id :hero])))

(defn get-times-hero-power-used
  "Returns the amount of times the given player has used a hero power."
  {:test (fn []
           (is= (get-times-hero-power-used (create-hero "Carl"))
                0))}
  [hero]
  (:times-hero-power-used hero))

(defn replace-minion
  "Replaces a minion with the same id as the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (replace-minion (create-minion "Ronja" :id "m"))
                    (get-minion "m")
                    (:name))
                "Ronja"))}
  [state new-minion]
  (let [owner-id (or (:owner-id new-minion)
                     (:owner-id (get-minion state (:id new-minion))))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (map (fn [m]
                        (if (= (:id m) (:id new-minion))
                          new-minion
                          m))
                      minions)))))

(defn update-minion
  "Updates the value of the given key for the minion with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion "m" :damage-taken inc)
                    (get-minion "m")
                    (:damage-taken))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion "m" :damage-taken 2)
                    (get-minion "m")
                    (:damage-taken))
                2))}
  [state id key function-or-value]
  (let [minion (get-minion state id)]
    (replace-minion state (if (fn? function-or-value)
                            (update minion key function-or-value)
                            (assoc minion key function-or-value)))))

(defn get-owner-id
  "Returns the player-id of the owner of the given minion-id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-owner-id "m"))
                "p1"))}
  [state minion-id]
  (:owner-id (get-minion state minion-id)))

(defn update-positions
  "Updates the minion positions on the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")
                                             (create-minion "Kato" :id "k")]}])
                    (update-in
                      [:players "p1" :minions]
                      (fn [minions]
                        (remove (fn [m] (= (:id m) "e")) minions)))
                    (update-positions "p1")
                    (get-minion "k")
                    (:position))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")
                                             (create-minion "Kato" :id "k")]}])
                    (update-in
                      [:players "p1" :minions]
                      (fn [minions]
                        (remove (fn [m] (or (= (:id m) "e") (= (:id m) "m"))) minions)))
                    (update-positions "p1")
                    (get-minion "r")
                    (:position))
                0))}

  [state owner-id]
  (reduce (fn [acc-state m]
            (replace-minion acc-state m))
          state
          (map-indexed
            (fn [index m]
              (assoc m :position index))
            (get-minions state owner-id))))

(defn get-dead-minions
  [state player-id]
  (:dead-minions (get-player state player-id)))

(defn add-dead-minion
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")]}]) $
                      (add-dead-minion $ (get-minion $ "m1"))
                      (:name (first (get-dead-minions $ "p1"))))
                "Mio"))}
  [state minion]
  (let [player-id (:owner-id minion)]
    (update-in state [:players player-id :dead-minions] conj minion)))



(defn remove-minion
  "Removes a minion with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (remove-minion "m")
                    (get-minions))
                [])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")
                                             (create-minion "Kato" :id "k")]}])
                    (remove-minion "r")
                    (get-minion "k")
                    (:position))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Ronja" :id "r")
                                             (create-minion "Kato" :id "k")]}])
                    (remove-minion "m")
                    (remove-minion "e")
                    (get-minion "r")
                    (:position))
                0))}
  [state id]
  (let [owner-id (:owner-id (get-minion state id))
        minion (get-minion state id)]
    (-> state
        (add-dead-minion minion)
        (update-in
          [:players owner-id :minions]
          (fn [minions]
            (remove (fn [m] (= (:id m) id)) minions)))
        (update-positions owner-id))))

(defn remove-minions
  "Removes the minions with the given ids from the state."
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                               (create-minion "Mio" :id "m2")]}
                                    {:minions [(create-minion "Mio" :id "m3")
                                               (create-minion "Mio" :id "m4")]}]) $
                      (remove-minions $ "m1" "m4")
                      (get-minions $)
                      (map :id $))
                ["m2" "m3"]))}
  [state & ids]
  (reduce remove-minion state ids))

(defn get-cards
  "Returns all of the cards for both players or the player with the specified id."
  ([state]
   (let [player-ids (->> (get-players state)
                         (map :id))]
     (apply concat (map (fn [player-id] (get-cards state player-id)) player-ids))))
  ([state player-id]
   (concat (get-hand state player-id)
           (get-deck state player-id))))

(defn get-card
  "Returns the card with the specified id."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (get-card "e")
                    (:name))
                "Emil"))}
  [state card-id]
  (->> (get-cards state)
       (filter (fn [c] (= (:id c) card-id)))
       (first)))

(defn remove-card-from-hand
  "Removes the card with the specified id from the specified player's hand."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")
                                          "Mio"
                                          "Ronja"]}])
                    (remove-card-from-hand "p1" "e")
                    (get-hand "p1")
                    (count))
                2))}
  [state player-id card-id]
  (update-in state [:players player-id :hand]
             (fn [cards]
               (->> cards
                    (remove (fn [c] (= (:id c) card-id)))))))


(defn remove-card-from-deck
  "Removes the card with the specified id from the given player's deck."
  {:test (fn []
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")
                                          "Mio"
                                          "Ronja"]}])
                    (remove-card-from-deck "p1" "e")
                    (get-deck "p1")
                    (count))
                2)
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e") (create-card "Mio" :id "m")]}])
                    (remove-card-from-deck "p1" "e")
                    (get-deck "p1")
                    (count))
                1))}
  [state player-id card-id]
  (update-in state [:players player-id :deck]
             (fn [cards]
               (->> cards
                    (remove (fn [c] (= (:id c) card-id)))))))

(defn replace-card-in-hand
  "Replaces a card in the hand with the same id as the given new card in the hand."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (replace-card-in-hand (create-card "Ronja" :id "m"))
                    (get-card "m")
                    (:name))
                "Ronja"))}
  [state new-card]
  (let [owner-id (or (:owner-id new-card)
                     (:owner-id (get-card state (:id new-card))))]
    (update-in state
               [:players owner-id :hand]
               (fn [cards]
                 (map (fn [m]
                        (if (= (:id m) (:id new-card))
                          new-card
                          m))
                      cards)))))

(defn replace-card-in-deck
  "Replaces a card in the deck with the same id as the given new card in the deck."
  {:test (fn []
           (is= (-> (create-game [{:deck [(create-card "Mio" :id "m")]}])
                    (replace-card-in-deck (create-card "Ronja" :id "m"))
                    (get-card "m")
                    (:name))
                "Ronja"))}
  [state new-card]
  (let [owner-id (or (:owner-id new-card)
                     (:owner-id (get-card state (:id new-card))))]
    (update-in state
               [:players owner-id :deck]
               (fn [cards]
                 (map (fn [m]
                        (if (= (:id m) (:id new-card))
                          new-card
                          m))
                      cards)))))

(defn update-card
  "Updates the value of the given key for the card with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (update-card "m" :name :hand "Ronja")
                    (get-card "m")
                    (:name))
                "Ronja")
           (is= (-> (create-game [{:deck [(create-card "Mio" :id "m")]}])
                    (update-card "m" :buffs :deck [{:mana-cost 2}])
                    (get-card "m")
                    (:buffs))
                [{:mana-cost 2}]))}
  [state id key hand-or-deck function-or-value]
  (let [card (get-card state id)]
    (if (= hand-or-deck :hand)
      (replace-card-in-hand state (if (fn? function-or-value)
                                    (update card key function-or-value)
                                    (assoc card key function-or-value)))
      (replace-card-in-deck state (if (fn? function-or-value)
                                    (update card key function-or-value)
                                    (assoc card key function-or-value))))))

(defn get-last-minion-position
  "Returns the most recently filled position on the given player's side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m")]}])
                    (get-last-minion-position "p1"))
                0)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m0") (create-minion "Mio" :owner-id "p1" :id "m1") (create-minion "Mio" :owner-id "p1" :id "m2") (create-minion "Mio" :owner-id "p1" :id "m3") (create-minion "Mio" :owner-id "p1" :id "m4") (create-minion "Mio" :owner-id "p1" :id "m5") (create-minion "Mio" :owner-id "p1" :id "m6")]}])
                    (get-last-minion-position "p1"))
                6))}

  [state player-id]
  (- (count (get-minions state player-id)) 1))

(defn next-minion-position
  "Returns the next available position on the given player's side of the board."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m")]}])
                    (next-minion-position "p1"))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :owner-id "p1" :id "m0") (create-minion "Mio" :owner-id "p1" :id "m1") (create-minion "Mio" :owner-id "p1" :id "m2") (create-minion "Mio" :owner-id "p1" :id "m3") (create-minion "Mio" :owner-id "p1" :id "m4") (create-minion "Mio" :owner-id "p1" :id "m5") (create-minion "Mio" :owner-id "p1" :id "m6")]}])
                    (next-minion-position "p1"))
                7))}

  [state player-id]
  (+ (get-last-minion-position state player-id) 1))

(defn minion-exists?
  "Returns whether or not the given minion exists on the board."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                   (minion-exists? "m")))
           (is-not (-> (create-game)
                       (minion-exists? "m"))))}

  [state minion-id]
  (not (nil? (get-minion state minion-id))))

(defn hero-exists?
  "Returns whether or not the given minion exists on the board."
  {:test (fn []
           (is (-> (create-game)
                   (hero-exists? "h1")))
           (is-not (-> (create-game)
                       (hero-exists? "h3"))))}

  [state hero-id]
  (not (nil? (get-hero state hero-id))))

(defn character-exists?
  "Determines whether or not the given character exists in the game."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                   (character-exists? "m")))
           (is-not (-> (create-game)
                       (character-exists? "m")))
           (is (-> (create-game)
                   (character-exists? "h2")))
           (is-not (-> (create-game)
                       (character-exists? "h4"))))}
  [state character-id]
  (or (minion-exists? state character-id)
      (hero-exists? state character-id)))

(defn switch-owner
  "Switches ownership of the specified minion to the specified player."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (switch-owner "m" "p2")
                    (get-minions "p2")
                    (count))
                1))}

  [state minion-id player-id]
  (let [old-minion (get-minion state minion-id)]

    (as-> state $
          (remove-minion $ minion-id)
          (add-minion-to-board $ player-id old-minion
                               (next-minion-position state player-id)))))

(defn inc-attacks-performed-minion
  "Exhausts the given minion by incrementing the value of the number of attacks they have performed this turn."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (inc-attacks-performed-minion "m")
                    (get-minion "m")
                    (:attacks-performed-this-turn))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (inc-attacks-performed-minion "m")
                    (inc-attacks-performed-minion "m")
                    (get-minion "m")
                    (:attacks-performed-this-turn))
                2))}
  [state minion-id]
  (if (minion-exists? state minion-id)
    (update-minion state minion-id :attacks-performed-this-turn inc)
    state))

(defn add-minion-buff
  "Adds the specified buff to the given minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (get-minion "e")
                    (:buffs))
                [{:attack 2 :health 2}]))}
  [state minion-id buff]
  (update-minion state minion-id :buffs
                 (fn [buffs] (conj buffs buff))))

(defn add-minion-buffs
  "Adds the given buffs to the specified minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buffs "e" [{:attack 2 :health 2} {:taunt true :health 3}])
                    (get-minion "e")
                    (:buffs))
                [{:attack 2 :health 2} {:taunt true :health 3}]))}
  [state minion-id buffs]
  (reduce (fn [acc-state b]
            (add-minion-buff acc-state minion-id b))
          state
          buffs))

(defn remove-minion-buff
  "Removes the given buff from the specified minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (remove-minion-buff "e" {:attack 2 :health 2})
                    (get-minion "e")
                    (:buffs))
                [])
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:taunt true})
                    (remove-minion-buff "e" :taunt)
                    (get-minion "e")
                    (:buffs))
                []))}
  [state minion-id buff]
  (if (map? buff)
    (update-minion state minion-id :buffs (fn [buffs] (remove (fn [b] (= b buff)) buffs)))
    (update-minion state minion-id :buffs (fn [buffs] (remove (fn [b] (contains? b buff)) buffs)))))

(defn get-minion-buffs
  "Returns the buffs of the given minion or id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (get-minion-buffs "e"))
                [{:attack 2 :health 2}])
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2 :duration 2})
                    (get-minion-buffs "e"))
                [{:attack 2 :health 2 :duration 2}])
           (is= (as-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}]) $
                      (add-minion-buff $ "e" {:taunt true})
                      (get-minion-buffs $ (get-minion $ "e")))
                [{:taunt true}])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-minion-buffs "m"))
                []))}

  [state minion-or-id]
  (if (string? minion-or-id)
    (:buffs (get-minion state minion-or-id))
    (:buffs minion-or-id)))

(defn has-aura-ability?
  "Determines whether or not the minion has the given aura-ability."
  {:test (fn []
           (let [state (create-game [{:minions [(create-minion "Tjorven" :id "t")
                                                (create-minion "Uncle Melker" :id "um")]}])]
             (is (has-aura-ability? state (get-minion state "um") :windfury))))}
  [state minion ability]
  (let [aura-abilities (reduce (fn [a m]
                                 (let [aura-buffs (first (filter (fn [b] (contains? b :aura)) (get-minion-buffs state m)))
                                       aura-fn (when aura-buffs
                                                 (:aura (get-definition (:aura aura-buffs))))]

                                   (if-not aura-fn
                                     a
                                     (let [result (aura-fn state m minion)]
                                       (if-not result
                                         a
                                         (conj a result))))))
                               []
                               (get-minions state))]
    (some (fn [b]
            (contains? b ability))
          aura-abilities)))

(defn has-ability?
  "Returns whether or not the given minion has the given ability."
  {:test (fn []
           (is-not (has-ability? (create-minion "Mio") :taunt))
           (is-not (has-ability? (create-minion "Jonatan") :divine-shield))
           (is (has-ability? (create-minion "Elisabeth") :taunt))
           (is (has-ability? (create-minion "Elisabeth") :divine-shield))
           (let [state (create-game [{:minions ["Tjorven"
                                                (create-minion "Uncle Melker" :id "um")]}])]
             (is (has-ability? state (get-minion state "um") :windfury))))}
  ([minion ability]
   (some (fn [a] (= a ability))
         (:abilities minion)))
  ([state minion ability]
   (or (has-ability? minion ability)
       (has-aura-ability? state minion ability))))

(defn get-abilities
  "Returns the abilities of the given minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (get-abilities "e"))
                [:divine-shield :taunt])
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-abilities "m"))
                []))}

  [state minion-id]
  (-> state
      (get-minion minion-id)
      (:abilities)))

(defn remove-ability
  "Removes the given ability from the specified minion."
  {:test (fn []
           (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                       (remove-ability "e" :taunt)
                       (get-minion "e")
                       (has-ability? :taunt)))
           (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                       (remove-ability "e" :divine-shield)
                       (get-minion "e")
                       (has-ability? :divine-shield))))}
  [state minion-id ability]
  (-> state
      (update-minion minion-id :abilities
                     (fn [abilities]
                       (->> abilities
                            (remove (fn [a] (= a ability))))))
      (remove-minion-buff minion-id ability)))

(defn add-ability
  "Adds the given ability to the specified minion."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                   (add-ability "m" :taunt)
                   (get-minion "m")
                   (has-ability? :taunt)))
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (add-ability "e" :taunt)
                    (add-ability "e" :taunt)
                    (get-abilities "e")
                    (count))
                2)
           (is (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                   (add-ability "k" :divine-shield)
                   (get-minion "k")
                   (has-ability? :divine-shield))))}
  [state minion-id ability]
  (update-minion state minion-id :abilities
                 (fn [abilities]
                   (-> abilities
                       (conj ability)
                       (distinct)))))

(defn minion-has-buff?
  "Determines whether or not the given minion has the given buff."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                   (add-minion-buff "e" {:attack 2 :health 2})
                   (minion-has-buff? "e" :attack)))
           (is (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                   (minion-has-buff? "e" :taunt)))
           (is (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")
                                            (create-minion "Tjorven" :id "t")]}])
                   (minion-has-buff? "e" :windfury))))}

  [state minion-or-id buff]
  (if (string? minion-or-id)
    (or (some (fn [b] (contains? b buff))
              (get-minion-buffs state minion-or-id))
        (has-ability? state (get-minion state minion-or-id) buff))
    (or (some (fn [b] (contains? b buff))
              (get-minion-buffs state minion-or-id))
        (has-ability? state minion-or-id buff))))

(defn divine-shield?
  "Returns whether or not the given minion has a divine shield."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}]) $
                     (divine-shield? $ (get-minion $ "um"))))
           (is-not (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                         (divine-shield? $ (get-minion $ "m")))))}
  ([state minion]
   (has-ability? state minion :divine-shield))
  ([minion]
   (has-ability? minion :divine-shield)))

(defn taunt?
  "Returns whether or not the given minion has taunt."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}]) $
                         (taunt? $ (get-minion $ "um"))))
           (is (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}]) $
                     (taunt? $ (get-minion $ "j")))))}
  ([state minion]
   (has-ability? state minion :taunt))
  ([minion]
   (has-ability? minion :taunt)))

(defn poisonous?
  "Returns whether or not the given minion has poisonous."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}]) $
                         (poisonous? $ (get-minion $ "um"))))
           (is (as-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}]) $
                     (poisonous? $ (get-minion $ "hn")))))}
  ([state minion]
   (has-ability? state minion :poisonous))
  ([minion]
   (has-ability? minion :poisonous)))

(defn windfury?
  "Returns whether or not the given minion has windfury."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}]) $
                         (windfury? $ (get-minion $ "um"))))
           (is (as-> (create-game [{:minions [(create-minion "Rasmus" :id "r")]}]) $
                     (windfury? $ (get-minion $ "r"))))
           (is (as-> (create-game [{:minions ["Tjorven"
                                              (create-minion "Uncle Melker" :id "um")]}]) $
                     (windfury? $ (get-minion $ "um")))))}
  ([state minion]
   (has-ability? state minion :windfury))
  ([minion]
   (has-ability? minion :windfury)))

(defn charge?
  "Returns whether or not the given minion has charge."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}]) $
                         (charge? $ (get-minion $ "um"))))
           (is (as-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "a")]}]) $
                     (charge? $ (get-minion $ "a")))))}
  ([state minion]
   (has-ability? state minion :charge))
  ([minion]
   (has-ability? minion :charge)))

(defn add-taunt
  "Adds taunt to the specified minion."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                     (add-taunt $ "m")
                     (taunt? $ (get-minion $ "m")))))}

  [state minion-id]
  (add-ability state minion-id :taunt))

(defn add-divine-shield
  "Adds a divine shield to the specified minion."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                     (add-divine-shield $ "m")
                     (divine-shield? $ (get-minion $ "m")))))}

  [state minion-id]
  (add-ability state minion-id :divine-shield))

(defn add-poisonous
  "Adds poisonous to the specified minion."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                     (add-poisonous $ "m")
                     (poisonous? $ (get-minion $ "m")))))}
  [state minion-id]
  (add-ability state minion-id :poisonous))

(defn add-windfury
  "Adds windfury to the specified minion."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                     (add-windfury $ "m")
                     (windfury? $ (get-minion $ "m")))))}
  [state minion-id]
  (add-ability state minion-id :windfury))

(defn add-charge
  "Adds charge to the specified minion."
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                     (add-charge $ "m")
                     (charge? $ (get-minion $ "m")))))}
  [state minion-id]
  (add-ability state minion-id :charge))

(defn remove-divine-shield
  "Removes a divine shield from the specified minion."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}]) $
                         (remove-divine-shield $ "e")
                         (divine-shield? $ (get-minion $ "e")))))}
  [state minion-id]
  (remove-ability state minion-id :divine-shield))

(defn remove-taunt
  "Removes taunt from the specified minion."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}]) $
                         (remove-taunt $ "e")
                         (taunt? $ (get-minion $ "e")))))}
  [state minion-id]
  (remove-ability state minion-id :taunt))


(defn remove-poisonous
  "Removes poisonous from the specified minion."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}]) $
                         (remove-poisonous $ "hn")
                         (poisonous? $ (get-minion $ "hn")))))}
  [state minion-id]
  (remove-ability state minion-id :poisonous))

(defn remove-windfury
  "Removes windfury from the specified minion."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Rasmus" :id "r")]}]) $
                         (remove-windfury $ "r")
                         (windfury? $ (get-minion $ "r")))))}
  [state minion-id]
  (remove-ability state minion-id :windfury))

(defn remove-charge
  "Removes charge from the specified minion."
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "a")]}]) $
                         (remove-charge $ "a")
                         (charge? $ (get-minion $ "a")))))}
  [state minion-id]
  (remove-ability state minion-id :charge))

(defn get-minion-buff-names
  "Gets the names of ability buffs from the given minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (add-minion-buff "e" {:windfury true :taunt true})
                    (add-minion-buff "e" {:windfury true :taunt true :divine-shield true})
                    (get-minion-buff-names "e"))
                [:windfury :taunt :divine-shield])
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (add-minion-buff "e" {:attack 2 :health 2})
                    (get-minion-buff-names "e"))
                []))}

  [state minion-or-id]
  (let [minion (if (string? minion-or-id)
                 (get-minion state minion-or-id)
                 minion-or-id)]

    (filter (fn [name] (not-any? (fn [x] (= name x)) [:attack :health :duration]))
            (distinct
              (reduce (fn [names buff]
                        (concat names (keys buff)))
                      []
                      (get-minion-buffs state minion))))))

(defn add-card-buff
  "Adds the specified buff to the given card."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (add-card-buff "e" {:attack 2 :health 2} :hand)
                    (get-card "e")
                    (:buffs))
                [{:attack 2 :health 2}]))}
  [state card-id buff hand-or-deck]
  (update-card state card-id :buffs hand-or-deck
               (fn [buffs] (conj buffs buff))))

(defn remove-card-buff
  "Removes the given buff from the specified card."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (add-card-buff "e" {:attack 2 :health 2} :hand)
                    (remove-card-buff "e" {:attack 2 :health 2} :hand)
                    (get-card "e")
                    (:buffs))
                [])
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (add-card-buff "e" {:taunt true} :deck)
                    (remove-card-buff "e" :taunt :deck)
                    (get-card "e")
                    (:buffs))
                []))}
  [state card-id buff hand-or-deck]
  (if (map? buff)
    (update-card state card-id :buffs hand-or-deck (fn [buffs] (remove (fn [b] (= b buff)) buffs)))
    (update-card state card-id :buffs hand-or-deck (fn [buffs] (remove (fn [b] (contains? b buff)) buffs)))))

(defn get-card-buffs
  "Returns the buffs of the given card or id."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Elisabeth" :id "e")]}])
                    (add-card-buff "e" {:attack 2 :health 2} :hand)
                    (get-card-buffs "e"))
                [{:attack 2 :health 2}])
           (is= (-> (create-game [{:hand [(create-card "Elisabeth" :id "e")]}])
                    (add-card-buff "e" {:attack 2 :health 2 :duration 2} :hand)
                    (get-card-buffs "e"))
                [{:attack 2 :health 2 :duration 2}])
           (is= (as-> (create-game [{:deck [(create-card "Elisabeth" :id "e")]}]) $
                      (add-card-buff $ "e" {:taunt true} :deck)
                      (get-card-buffs $ (get-card $ "e")))
                [{:taunt true}])
           (is= (-> (create-game [{:deck [(create-card "Mio" :id "m")]}])
                    (get-card-buffs "m"))
                []))}

  [state card-or-id]
  (if (string? card-or-id)
    (:buffs (get-card state card-or-id))
    (:buffs card-or-id)))

(defn is-current-hero?
  "Returns whether or not the specified hero is the hero of the player whose turn it is."
  {:test (fn []
           (is (-> (create-game)
                   (is-current-hero? "h1")))
           (is-not (-> (create-game)
                       (is-current-hero? "h2"))))}
  [state id]
  (= (get-hero state id) (:hero (get-player state (get-player-id-in-turn state)))))

(defn remove-secret
  "Removes the secret with the specified name from the given player's secrets."
  {:test (fn []
           (is= (-> (create-game [{:secrets ["Explosive Trap"]}])
                    (remove-secret "p1" "Explosive Trap")
                    (get-secrets "p1"))
                [])
           (is= (-> (create-game [{:hand    [(create-card "Vaporize" :id "v")]
                                   :secrets ["Explosive Trap" "Venomstrike Trap"]}])
                    (remove-secret "p1" "Explosive Trap")
                    (get-secrets "p1"))
                [{:name "Venomstrike Trap", :id "s2", :owner-id "p1", :entity-type :secret}])
           (is= (-> (create-game [{:secrets ["Explosive Trap"]}])
                    (remove-secret "p1" "Venomstrike Trap")
                    (get-secrets "p1"))
                [{:name "Explosive Trap", :id "s1", :owner-id "p1", :entity-type :secret}]))}
  [state player-id secret-name]
  (update-in state [:players player-id :secrets]
             (fn [secrets]
               (remove (fn [s] (= (:name s) secret-name))
                       secrets))))

(defn switch-secret-owner
  "Switches ownership of the specified secret to the specified player."
  {:test (fn []
           (is= (-> (create-game [{:secrets ["Explosive Trap"]}])
                    (switch-secret-owner "Explosive Trap" "p2")
                    (get-secrets "p2"))
                [{:name "Explosive Trap", :id "s2", :owner-id "p2", :entity-type :secret}]))}
  [state secret-name player-id]
  (-> state
      (remove-secret (get-enemy-player-id state player-id) secret-name)
      (add-secret player-id secret-name)))

(defn get-enemy-secrets
  "Returns the secrets of the opponent of the given player."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-secrets "p1"))
                []))}
  [state player-id]
  (let [enemy-player-id (get-enemy-player-id state player-id)]
    (get-in state [:players enemy-player-id :secrets])))

(defn get-all-secrets
  "Returns all of the secrets of the entire board (the given player and their opponent)."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-secrets "p1"))
                []))}
  [state player-id]
  (concat (get-secrets state player-id)
          (get-enemy-secrets state player-id)))




