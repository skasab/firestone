(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-card
                                         create-hero
                                         create-game]]
            [firestone.core-api :refer [attack
                                        end-turn
                                        play-minion-card
                                        play-spell-card
                                        use-hero-power]]
            [firestone.client.mapper :refer [get-client-state]]
            [firestone.definitions :refer [get-definitions]]))

(defonce game-atom (atom nil))

(defn create-game!
  []
  (let [card-definitions (->> (get-definitions)
                              (filter (fn [d] (or (= (:type d) :minion)
                                                  (= (:type d) :spell)))))]
    (get-client-state (first (:states (reset! game-atom
                                              {:states      (list (create-game [{:deck (->> card-definitions
                                                                                            (map (fn [d] (create-card (:name d))))
                                                                                            (shuffle))
                                                                                 :hand [(create-card "Spellbreaker")
                                                                                        (create-card "Al'Akir the Windlord")
                                                                                        (create-card "Stormwind Knight")
                                                                                        (create-card "Leeroy Jenkins")
                                                                                        (create-card "The Mistcaller")
                                                                                        (create-card "Shudderwock")
                                                                                        (create-card "Secretkeeper")
                                                                                        (create-card "Eater of Secrets")
                                                                                        (create-card "Kezan Mystic")
                                                                                        (create-card "Mad Scientist")
                                                                                        (create-card "Silence")
                                                                                        (create-card "Explosive Trap")
                                                                                        (create-card "Venomstrike Trap")
                                                                                        (create-card "Vaporize")]}
                                                                                {:deck (->> card-definitions
                                                                                            (map (fn [d] (create-card (:name d))))
                                                                                            (shuffle))
                                                                                 :hero (create-hero "Gustaf")

                                                                                 :hand [(create-card "Spellbreaker")
                                                                                        (create-card "Al'Akir the Windlord")
                                                                                        (create-card "Stormwind Knight")
                                                                                        (create-card "Leeroy Jenkins")
                                                                                        (create-card "The Mistcaller")
                                                                                        (create-card "Shudderwock")
                                                                                        (create-card "Secretkeeper")
                                                                                        (create-card "Eater of Secrets")
                                                                                        (create-card "Kezan Mystic")
                                                                                        (create-card "Mad Scientist")
                                                                                        (create-card "Silence")
                                                                                        (create-card "Explosive Trap")
                                                                                        (create-card "Venomstrike Trap")
                                                                                        (create-card "Vaporize")]}]))
                                               :redo-states (list)}))))))


(defn action!
  [action & values]
  (-> (swap! game-atom (fn [game]
                         (-> game
                             (update :states (fn [old] (conj old
                                                             (apply action (first old) values))))
                             (assoc :redo-states (list)))))
      (:states)
      (first)
      (get-client-state)))

(defn end-turn!
  []
  (action! end-turn))

(defn attack!
  [player-id attacker-id target-id]
  (action! attack player-id attacker-id target-id))

(defn play-minion-card!
  [player-id card-id target-id]
  (action! play-minion-card player-id card-id target-id))

(defn play-spell-card!
  [player-id card-id target-id]
  (action! play-spell-card player-id card-id target-id))


(defn use-hero-power!
  [player-id target-id]
  (action! use-hero-power player-id target-id))

(defn undo!
  []
  (-> (swap! game-atom (fn [game]
                         (-> game
                             (update :redo-states (fn [old] (conj old
                                                                  (first (:states game)))))
                             (update :states pop))))
      (:states)
      (first)
      (get-client-state)))

(defn redo!
  []
  (-> (swap! game-atom (fn [game]
                         (-> game
                             (update :states (fn [old] (conj old
                                                             (first (:redo-states game)))))
                             (update :redo-states pop))))
      (:states)
      (first)
      (get-client-state)))

