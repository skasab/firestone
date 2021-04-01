(ns firestone.client.mapper
  (:require [ysera.test :refer [is is-not is=]]
            [firestone.client.spec]
            [firestone.construct :refer [create-card
                                         create-game
                                         create-minion
                                         get-battlecries
                                         get-minion-buff-names
                                         get-card
                                         get-hand
                                         get-hero
                                         get-minion
                                         get-minions
                                         get-player
                                         get-players
                                         get-secret
                                         get-secrets]]
            [firestone.core :refer [get-attack
                                    get-characters
                                    get-card-mana-cost
                                    get-health
                                    get-max-health
                                    get-minion-card-attack
                                    get-minion-card-health
                                    sleepy?]]
            [firestone.core-api :refer [can-use-hero-power
                                        get-valid-minion-attack-ids
                                        get-valid-target-ids
                                        play-minion-card
                                        playable-minion-card?
                                        playable-spell-card?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.spec.alpha :as spec]
            [firestone.definitions-loader]))

(defn check-spec
  [spec value]
  (let [valid (spec/valid? spec value)]
    (if valid
      true
      (do (spec/explain spec value)
          false))))



(defn get-client-hero-power
  {:test (fn []
           (is (check-spec :firestone.client.spec/hero-power
                           (let [game (create-game [{:hand [(create-card "Mio" :id "m")]}])
                                 hero (get-hero game "h1")
                                 hero-power (:hero-power (get-definition "Carl"))]
                             (get-client-hero-power game hero hero-power)))))}
  [state hero hero-power]
  {:can-use            (can-use-hero-power hero hero-power)
   :owner-id           (:owner-id hero)
   :entity-type        "hero-power"
   :has-used-your-turn (pos? (:times-hero-power-used hero))
   :name               (:name (get-definition hero-power))
   :description        (:description (get-definition hero-power))
   :mana-cost          (:mana-cost (get-definition hero-power))
   :original-mana-cost (:mana-cost (get-definition hero-power))
   :valid-target-ids   (map :id (get-characters state))
   })


(defn get-client-spell-card

  [state player spell-card]
  {:owner-id         (:id player)
   :id               (:id spell-card)
   :valid-target-ids (if (:needs-targets (get-definition spell-card))
                       (get-valid-target-ids state (:name (get-definition spell-card)))
                       [])
   :playable         (playable-spell-card? state player spell-card)
   :description      (or (:description (get-definition spell-card)
                           ""))
   })

(defn get-client-minion-card

  [state player minion-card]
  {:owner-id         (:id player)
   :id               (:id minion-card)
   :attack           (get-minion-card-attack minion-card)
   :original-attack  (:attack (get-definition minion-card))
   :health           (get-minion-card-health minion-card)
   :original-health  (:health (get-definition minion-card))
   :valid-target-ids (if (:needs-targets (get-definition minion-card))
                       (get-valid-target-ids state (:name (get-definition minion-card)))
                       [])
   :playable         (playable-minion-card? state player minion-card)
   :description      (or (:description (get-definition minion-card)
                           ""))})

(defn get-client-card
  {:test (fn []
           (is (check-spec :firestone.client.spec/card
                           (let [game (create-game [{:hand [(create-card "Mio" :id "m")]}])
                                 player (get-player game "p1")
                                 card (get-card game "m")]
                             (get-client-card game player card)))))}
  [state player card]
  (let [card-type (name (:type (get-definition card)))
        base-map {:entity-type        "card"
                  :name               (:name card)
                  :mana-cost          (get-card-mana-cost card)
                  :original-mana-cost (:mana-cost (get-definition card))
                  :type               card-type}]
    (cond
      (= card-type "minion")
      (merge base-map (get-client-minion-card state player card))

      (= card-type "spell")
      (merge base-map (get-client-spell-card state player card)))))

(defn get-client-secret
  {:test (fn []
           (is (check-spec :firestone.client.spec/secret
                           (let [game (create-game [{:secrets ["Vaporize"]}])
                                 player (get-player game "p1")
                                 secret (get-secret game "p1" "Vaporize")]
                             (get-client-secret game player secret)))))}
  [state player secret]
  (let [definition (get-definition (:name secret))]
    {:name               (:name definition)
     :owner-id           (:owner-id secret)
     :id                 (:id secret)
     :entity-type        "secret"
     :original-mana-cost (:mana-cost definition)
     :description        (:description definition)}))

(defn get-client-hero
  {:test (fn []
           (is (check-spec :firestone.client.spec/hero
                           (let [game (create-game)
                                 player (get-player game "p1")
                                 hero (:hero player)]
                             (get-client-hero game player hero)))))}
  [state player hero]
  {:armor            0
   :owner-id         (:id player)
   :entity-type      "hero"
   :attack           0
   :can-attack       false
   :health           (get-health hero)
   :hero-power       (get-client-hero-power state hero (:hero-power (get-definition hero)))
   :id               (:id hero)
   :mana             (:mana player)
   :max-health       30
   :max-mana         (:max-mana player)
   :name             (:name hero)
   :states           []
   :valid-attack-ids []})

(defn get-client-minion
  {:test (fn []
           (is (check-spec :firestone.client.spec/minion
                           (let [game (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                                 player (get-player game "p1")
                                 minion (get-minion game "m")]
                             (get-client-minion game player minion)))))}

  [state player minion]
  {:attack           (get-attack minion)
   :can-attack       (not (empty? (get-valid-minion-attack-ids state minion)))
   :description      (or (:description (get-definition minion)
                           ""))
   :entity-type      "minion"
   :health           (get-health minion)
   :id               (:id minion)
   :name             (:name (get-definition minion))
   :mana-cost        (:mana-cost (get-definition minion))
   :max-health       (get-max-health minion)
   :original-attack  (:attack (get-definition minion))
   :original-health  (:health (get-definition minion))
   :owner-id         (:owner-id minion)
   :position         (:position minion)
   :sleepy           (sleepy? state (:id minion))
   :states           (concat (get-minion-buff-names state minion) (:abilities minion))
   :valid-attack-ids (get-valid-minion-attack-ids state minion)
   })


(defn get-client-player
  {:test (fn []
           (is (check-spec :firestone.client.spec/player
                           (as-> (create-game) $
                                 (get-client-player $ (get-player $ "p1"))))))}
  [state player]
  {:board-entities (->> (get-minions state (:id player))
                        (map (fn [minion] (get-client-minion state player minion))))
   :active-secrets (->> (get-secrets state (:id player))
                        (map (fn [secret] (get-client-secret state player secret))))
   :deck-size      (count (:deck player))
   :hand           (->> (get-hand state (:id player))
                        (map (fn [card] (get-client-card state player card))))
   :hero           (get-client-hero state player (:hero player))
   :id             (:id player)
   :battlecries    (get-battlecries state (:id player))})

(defn get-client-state
  {:test (fn []
           (is (check-spec :firestone.client.spec/game-states
                           (-> (create-game)
                               (get-client-state)))))}
  [state]
  [{:id             "the-game-id"
    :player-in-turn (:player-id-in-turn state)
    :players        (->> (get-players state)
                         (map (fn [p]
                                (get-client-player state p))))
    :supports-undo  true
    :supports-redo  true
    :action-index   (:action-index state)
    :turn-index     (:turn-index state)}])

