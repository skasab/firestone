(ns firestone.definition.hero-power
  (:require [firestone.definitions :as definitions]
            [firestone.construct :refer [add-minion-buff
                                         add-divine-shield
                                         get-minions
                                         get-player-id-in-turn
                                         update-minion]]
            [firestone.core :refer [damage]]
            [firestone.random :refer [get-random-enemy-minion-id
                                      get-n-random-enemy-minion-ids
                                      get-random-friendly-minion-id
                                      get-n-random-friendly-minion-ids
                                      get-random-minion-id
                                      get-n-random-minion-ids]]))

(def hero-definitions
  {

   "Blessing"
   {:name        "Blessing"
    :type        :hero-power
    :mana-cost   2
    :description "Give a minion Divine Shield."
    :max-uses     1
    :power       (fn [state minion-id]
                   (add-divine-shield state minion-id))}

   "Strengthen"
   {:name        "Strengthen"
    :type        :hero-power
    :mana-cost   3
    :description "Deal 1 damage to two random friendly minions and give them +2 Attack."
    :max-uses     1
    :power       (fn [state _]
                   (let [player-id (get-player-id-in-turn state)
                         [state minions-ids] (get-n-random-friendly-minion-ids state player-id 2)
                         minion1-id (first minions-ids)
                         minion2-id (second minions-ids)]
                     (if (and (not (nil? minion1-id)) (not (nil? minion2-id)))
                       (-> state
                           (add-minion-buff minion1-id {:attack 2})
                           (add-minion-buff minion2-id {:attack 2})
                           (damage minion1-id 1)
                           (damage minion2-id 1))
                       (if (and (not (nil? minion1-id)) (nil? minion2-id))
                         (-> state
                             (add-minion-buff minion1-id {:attack 2})
                             (damage minion1-id 1))
                         state))))}
   })

(definitions/add-definitions! hero-definitions)