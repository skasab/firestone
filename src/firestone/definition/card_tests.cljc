(ns firestone.definition.card-tests
  (:require [ysera.test :refer [deftest is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [add-minion-to-board
                                         charge?
                                         create-card
                                         create-game
                                         create-hero
                                         create-minion
                                         divine-shield?
                                         get-card
                                         get-card-buffs
                                         get-hand
                                         get-hero
                                         get-minion
                                         get-minion-buffs
                                         get-minions
                                         get-secrets
                                         minion-exists?
                                         poisonous?
                                         remove-minion
                                         taunt?
                                         windfury?]]
            [firestone.core :refer [damage
                                    damage-all-in-list
                                    get-attack
                                    get-characters
                                    get-health
                                    summon
                                    valid-attack?]]
            [firestone.core-api :refer [attack
                                        end-turn
                                        play-minion-card
                                        play-spell-card
                                        restore-mana
                                        use-hero-power]]
            ))


; Minion Card

(deftest Kato
         (is= (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                  (play-minion-card "p1" "k")
                  (get-health "h2"))
              26))

(deftest Emil
         (is= (-> (create-game [{:deck [(create-card "Mio" :id "m")]
                                 :hand [(create-card "Emil" :id "e")]}])
                  (play-minion-card "p1" "e")
                  (get-hand "p1")
                  (count))
              1))

(deftest Jonatan
         (is (-> (create-game [{:hand [(create-card "Jonatan" :id "j")]}])
                 (play-minion-card "p1" "j")
                 (get-minion "m1")
                 (taunt?))))

(deftest Alfred
         (is-not (-> (create-game [{:hand [(create-card "Alfred" :id "a")]}])
                     (play-minion-card "p1" "a")
                     (valid-attack? "p1" "m1" "h2"))))

(deftest Uncle-Melker
         (is (-> (create-game [{:hand [(create-card "Uncle Melker" :id "um")]}])
                 (play-minion-card "p1" "um")
                 (get-minion "m1")
                 (divine-shield?))))

(deftest Pippi
         (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "pip1")
                                           (create-minion "Pippi" :id "pip2")]}])
                  (end-turn)
                  (get-health "pip1"))
              3)
         (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "pip1")
                                           (create-minion "Pippi" :id "pip2")]}])
                  (end-turn)
                  (get-health "pip2"))
              3))

(deftest Karlsson
         (is
           (let [state (create-game [{:minions ["Karlsson"
                                                "Emil"]}])]
             (as-> state $
                   (end-turn $)
                   (get-minions $)
                   (some taunt? $)))))

(deftest Uncle-Nilsson
         (is= (as-> (create-game [{:minions [(create-minion "Uncle Nilsson" :id "un") "Emil"]}
                                  {:minions ["Mio" "Ronja"]}]) $
                    (damage $ (get-minion $ "un") 10)
                    (get-minions $ "p1")
                    (map :name $))
              ["Emil" "Mio"]))

(deftest Elisabeth
         (is (and (is (-> (create-game [{:hand [(create-card "Elisabeth" :id "e")]}])
                          (play-minion-card "p1" "e")
                          (get-minion "m1")
                          (taunt?)))
                  (is (-> (create-game [{:hand [(create-card "Elisabeth" :id "e")]}])
                          (play-minion-card "p1" "e")
                          (get-minion "m1")
                          (divine-shield?))))))

(deftest Madicken
         (is= (let [state (create-game [{:minions [(create-minion "Madicken" :id "m")
                                                   (create-minion "Emil")]}])]
                (as-> state $
                      (damage $ (get-minion $ "m") 10)
                      (count (get-minions $ "p1"))))
              2))

(deftest Ida
         (is
           (let [state (create-game [{:minions [(create-minion "Ida")
                                                (create-minion "Emil")]}])]
             (as-> state $
                   (damage $ (get-minion $ "m3") 1)
                   (taunt? (get-minion $ "m1"))))))

(deftest Herr-Nilsson
         (is-not (as-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}
                                     {:minions [(create-minion "Tjorven" :id "t")]}]) $
                       (attack $ "p1" "hn" "t")
                       (minion-exists? $ "t")))
         (is-not (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Herr Nilsson" :id "hn")]}]) $
                       (attack $ "p1" "m" "hn")
                       (minion-exists? $ "m"))))
(deftest Rasmus
         (is= (as-> (create-game [{:minions [(create-minion "Rasmus" :id "r")]}]) $
                    (attack $ "p1" "r" "h2")
                    (attack $ "p1" "r" "h2")
                    (get-health (get-hero $ "h2")))
              24))

(deftest Astrid
         (is= (-> (create-game [{:deck    [(create-card "Astrid" :id "a")]
                                 :minions [(create-minion "Madicken" :id "m")]}])
                  (play-minion-card "p1" "a" "m")
                  (damage "m2" 10)
                  (get-minions "p1")
                  (count))
              2)
         (is= (-> (create-game [{:deck    [(create-card "Astrid" :id "a")]
                                 :minions [(create-minion "Uncle Nilsson" :id "un")]}
                                {:minions [(create-minion "Mio")]}])
                  (play-minion-card "p1" "a" "un")
                  (damage "m2" 10)
                  (get-minions "p1")
                  (count))
              2))

(deftest Tjorven
         (is= (as-> (create-game [{:minions [(create-minion "Tjorven" :id "t")
                                             (create-minion "Elisabeth" :id "e")]}]) $
                    (attack $ "p1" "e" "h2")
                    (attack $ "p1" "e" "h2")
                    (get-health (get-hero $ "h2")))
              28)
         (error? (as-> (create-game [{:minions [(create-minion "Tjorven" :id "t")
                                                (create-minion "Elisabeth" :id "e")]}]) $
                       (attack $ "p1" "e" "h2")
                       (damage $ "t" 10)
                       (attack $ "p1" "e" "h2"))))

(deftest Skrallan
         (is= (as-> (create-game [{:minions [(create-minion "Skrallan" :id "s")
                                             (create-minion "Elisabeth" :id "e")]}]) $
                    (damage $ "e" 3)
                    (get-attack (get-minion $ "s")))
              4)
         (is= (as-> (create-game [{:minions [(create-minion "Skrallan" :id "s")
                                             (create-minion "Elisabeth" :id "e")]}]) $
                    (damage $ "e" 3)
                    (get-health (get-minion $ "s")))
              4)
         (is= (as-> (create-game [{:minions [(create-minion "Skrallan" :id "s")]}
                                  {:minions [(create-minion "Elisabeth" :id "e")]}]) $
                    (damage $ "e" 3)
                    (get-attack (get-minion $ "s")))
              2))

(deftest Annika
         (is= (as-> (create-game [{:hand [(create-card "Annika" :id "a")]}
                                  {:minions [(create-minion "Emil" :id "e")]}]) $
                    (play-minion-card $ "p1" "a" "e")
                    (get-attack (get-minion $ "e")))
              4)
         (is= (as-> (create-game [{:hand [(create-card "Annika" :id "a")]}
                                  {:minions [(create-minion "Emil" :id "e")]}]) $
                    (play-minion-card $ "p1" "a" "e")
                    (end-turn $)
                    (get-attack (get-minion $ "e")))
              2))

(deftest Al'Akir-the-Windlord
         (is (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "aa")]}])
                 (get-minion "aa")
                 (windfury?)))
         (is (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "aa")]}])
                 (get-minion "aa")
                 (charge?)))
         (is (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "aa")]}])
                 (get-minion "aa")
                 (divine-shield?)))
         (is (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "aa")]}])
                 (get-minion "aa")
                 (taunt?))))



(deftest Secretkeeper
         (is= (-> (create-game [{:minions [(create-minion "Secretkeeper" :id "sk")]
                                 :hand    [(create-card "Vaporize" :id "v")]}])
                  (play-spell-card "p1" "v")
                  (get-minion-buffs "sk"))
              [{:on-secret-play "Secretkeeper"} {:attack 1, :health 1}]))

(deftest Mad-Scientist
         (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]
                                   :deck    [(create-card "Vaporize" :id "v0")]}
                                  {:minions [(create-minion "Mad Scientist" :id "ms")]
                                   :deck    [(create-card "Explosive Trap" :id "et")]}]) $
                    (attack $ "p1" "e" "ms")
                    (count (get-secrets $ "p2")))
              1))


(deftest Eater-of-Secrets
         (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")]}
                                {:hand [(create-card "Eater of Secrets" :id "es")]}])
                  (play-spell-card "p1" "v")
                  (end-turn)
                  (play-minion-card "p2" "es")
                  (get-minion-buffs "m2"))
              [{:attack 1 :health 1}])
         (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")]}
                                {:hand [(create-card "Eater of Secrets" :id "es")]}])
                  (play-spell-card "p1" "v")
                  (end-turn)
                  (play-minion-card "p2" "es")
                  (get-secrets "p1")
                  (count))
              0))

(deftest Kezan-Mystic
         (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")
                                        (create-card "Explosive Trap" :id "et")]}
                                {:hand [(create-card "Kezan Mystic" :id "km")]}])
                  (play-spell-card "p1" "v")
                  (play-spell-card "p1" "et")
                  (end-turn)
                  (play-minion-card "p2" "km")
                  (get-secrets "p2")
                  (count))
              1)
         (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")
                                        (create-card "Explosive Trap" :id "et")]}
                                {:hand [(create-card "Kezan Mystic" :id "km")]}])
                  (end-turn)
                  (play-minion-card "p2" "km")
                  (get-secrets "p2")
                  (count))
              0)
         (is= (-> (create-game [{:hand [(create-card "Vaporize" :id "v")
                                        (create-card "Explosive Trap" :id "et")]}
                                {:hand [(create-card "Kezan Mystic" :id "km")]}])
                  (play-spell-card "p1" "v")
                  (play-spell-card "p1" "et")
                  (end-turn)
                  (play-minion-card "p2" "km")
                  (get-secrets "p1")
                  (count))
              1))

(deftest Stormwind-Knight
         (is (-> (create-game [{:minions [(create-minion "Stormwind Knight" :id "sk")]}])
                 (get-minion "sk")
                 (charge?))))

(deftest Leeroy-Jenkins
         (is (-> (create-game [{:minions [(create-minion "Leeroy Jenkins" :id "lj")]}])
                 (get-minion "lj")
                 (charge?)))
         (is= (-> (create-game [{:hand [(create-minion "Leeroy Jenkins" :id "lj")]}])
                  (play-minion-card "p1" "lj")
                  (get-minions "p2")
                  (count))
              2))

(deftest The-Mistcaller
         (is= (-> (create-game [{:hand [(create-card "The Mistcaller" :id "tm")
                                        (create-card "Mio" :id "m")]}])
                  (play-minion-card "p1" "tm")
                  (get-card-buffs "m"))
              [{:attack 1 :health 1}]))

(deftest Spellbreaker
         (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}
                                   {:hand [(create-minion "Spellbreaker" :id "sb")]}])
                     (end-turn)
                     (play-minion-card "p2" "sb" "e")
                     (taunt? "e")))
         (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "p")]}
                                {:hand [(create-minion "Spellbreaker" :id "sb")]}])
                  (end-turn)
                  (play-minion-card "p2" "sb" "p")
                  (get-minion-buffs "p"))
              [{:silenced true}])
         (is= (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "aatk")]}
                                {:hand [(create-minion "Spellbreaker" :id "sb")]}])
                  (end-turn)
                  (play-minion-card "p2" "sb" "aatk")
                  (get-minion-buffs "aatk"))
              [{:silenced true}]))

(deftest Shudderwock
         (as-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}
                             {:hand [(create-minion "Kato" :id "k")
                                     (create-minion "Emil" :id "e")
                                     (create-minion "Shudderwock" :id "sw")]
                              :deck [(create-card "Emil")
                                     (create-card "Ronja")]}]) $
               (end-turn $)
               (play-minion-card $ "p2" "k")
               (play-minion-card $ "p2" "e")
               (restore-mana $ "p2")
               (play-minion-card $ "p2" "sw")
               (do
                 (is= (get-health $ "h1") 22)
                 (is= (count (get-hand $ "p2")) 2))))

(deftest Emperor-Cobra
         (is (-> (create-game [{:minions [(create-minion "Emperor Cobra" :id "ec")]}])
                 (get-minion "ec")
                 (poisonous?))))

; Spell Cards

(deftest Insect-Swarm
         (is
           (let [characters
                 (-> (create-game [{:hand [(create-card "Insect Swarm" :id "i")]}
                                   {:minions [(create-minion "Emil") (create-minion "Alfred") (create-minion "Jonatan")]}])
                     (play-spell-card "p1" "i")
                     (get-characters))]
             (is=
               (filter (fn [x] (= (:damage-taken x) 2)) characters)
               characters))))

(deftest Radar-Raid
         (is= (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")]}])
                  (play-spell-card "p1" "r" "h2")
                  (get-health "h2"))
              27)
         (is= (-> (create-game [{:hand    [(create-card "Radar Raid" :id "r")]
                                 :minions [(create-minion "Uncle Melker" :id "un")]}])
                  (play-spell-card "p1" "r" "un")
                  (get-health "un"))
              5))

(deftest Silence
         (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}
                                   {:hand [(create-card "Silence" :id "s")]}])
                     (end-turn)
                     (play-spell-card "p2" "s" "e")
                     (taunt? "e")))
         (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "p")]}
                                {:hand [(create-card "Silence" :id "s")]}])
                  (end-turn)
                  (play-spell-card "p2" "s" "p")
                  (get-minion-buffs "p"))
              [{:silenced true}])
         (is= (-> (create-game [{:minions [(create-minion "Al'Akir the Windlord" :id "aatk")]}
                                {:hand [(create-card "Silence" :id "s")]}])
                  (end-turn)
                  (play-spell-card "p2" "s" "aatk")
                  (get-minion-buffs "aatk"))
              [{:silenced true}]))

; Secrets

(deftest Explosive-Trap
         (as-> (create-game [{:hand [(create-card "Explosive Trap" :id "et")]}
                             {:minions [(create-minion "Emil" :id "e")]}]) $
               (play-spell-card $ "p1" "et")
               (end-turn $)
               (attack $ "p2" "e" "h1")
               (do
                 (is= (get-health $ "h2") 27)
                 (is= (get-health $ "e") 3)
                 (is= (count (get-secrets $ "p1")) 0))))

(deftest Vaporize
         (as-> (create-game [{:hand [(create-card "Vaporize" :id "v")]}
                             {:minions [(create-minion "Emil" :id "e")]}]) $
               (play-spell-card $ "p1" "v")
               (end-turn $)
               (attack $ "p2" "e" "h1")
               (do
                 (is (not (minion-exists? $ "e")))
                 (is= (count (get-secrets $ "p1")) 0))))

(deftest Venomstrike-Trap
         (as-> (create-game [{:hand    [(create-card "Venomstrike Trap" :id "vs")]
                              :minions [(create-minion "Emil" :id "e")]}
                             {:minions [(create-minion "Mio" :id "m")]}]) $
               (play-spell-card $ "p1" "vs")
               (end-turn $)
               (attack $ "p2" "m" "e")
               (do
                 (is= (count (get-minions $ "p1")) 2)
                 (is= (count (get-secrets $ "p1")) 0))))

; Hero Powers

(deftest Blessing
         (is
           (let [state (create-game [{:minions [(create-minion "Ida")
                                                (create-minion "Emil")]}])]
             (as-> state $
                   (use-hero-power $ "p1" "m1")
                   (divine-shield? (get-minion $ "m1"))))))

(deftest Strengthen
         (let [state (-> (create-game [{:minions ["Ida" "Emil"]}
                                       {:hero    (create-hero "Gustaf")
                                        :minions [(create-minion "Ronja" :id "r")
                                                  "Mio" "Emil" "Kato"]}])
                         (end-turn)
                         (use-hero-power "p2"))]
           (is= (-> (get-minion state "r")
                    (get-attack))
                5)
           (is= (-> (get-minion state "r")
                    (get-health))
                1)))

;Sprint 5 cards

(deftest Acolyte-of-Pain
         ;Whenever this minion takes damage, draw a card.
         (is= (as-> (create-game [{:minions [(create-minion "Ida" :id "i")]}
                                  {:minions [(create-minion "Acolyte of Pain" :id "aop")]
                                   :hand    [(create-card "Mio")]
                                   :deck    [(create-card "Kato")]}]) $
                    (attack $ "p1" "i" "aop")
                    (count (get-hand $ "p2")))
              2))

(deftest Flesheating-Ghoul
         ;Whenever a minion dies, gain +1 Attack.
         (is= (as-> (create-game [{:minions [(create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Flesheating Ghoul" :id "fg")
                                             (create-minion "Mio" :id "m")]}]) $
                    (attack $ "p1" "r" "m")
                    (get-minion-buffs $ "fg"))
              [{:when-minion-dies "Flesheating Ghoul"} {:attack 1}]))

(deftest Snake-Trap
         ;Secret: When one of your minions is attacked summon three 1/1 Snakes.
         (is= (as-> (create-game [{:minions [(create-minion "Ronja" :id "r")]}
                                  {:secrets ["Snake Trap"]
                                   :minions [(create-minion "Mio" :id "m")]}]) $
                    (attack $ "p1" "r" "m")
                    (get-minions $ "p2")
                    (map :name $))
              ["Snake" "Snake" "Snake"])
         ;Shouldn't summon snakes if not your minion that was attacked.
         (is= (as-> (create-game [{:secrets ["Snake Trap"]
                                   :minions [(create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Mio" :id "m")]}]) $
                    (attack $ "p1" "r" "m")
                    (get-minions $ "p1")
                    (map :name $))
              ["Ronja"]))

(deftest Hadronox
         ;Deathrattle: Summon your Taunt minions that died this game.
         (is= (as-> (create-game [{:minions [(create-minion "Kato" :id "k")
                                             (create-minion "Kato" :id "k2")]}
                                  {:minions [(create-minion "Elisabeth" :id "e")
                                             (create-minion "Al'Akir the Windlord" :id "aa")
                                             (create-minion "Hadronox" :id "h")]}]) $
                    (remove-minion $ "aa")
                    (remove-minion $ "e")
                    (attack $ "p1" "k" "h")
                    (attack $ "p1" "k2" "h")
                    (get-minions $ "p2")
                    (map :name $))
              ["Al'Akir the Windlord" "Elisabeth"]))

(deftest Knife-Juggler
         ;After you summon a minion, deal 1 damage to a random enemy.
         (is= (as-> (create-game [{:minions [(create-minion "Knife Juggler" :id "k")]}
                                  {:minions [(create-minion "Mio" :id "m")]}]) $
                    (summon $ "p1" "Emil")
                    (get-health $ "m"))
              1)


         ;; If Kato attacks Knife Juggler, the attack should not work
         (is= (as-> (create-game [{:minions [(create-minion "Kato" :id "k")]}
                                  {:secrets ["Snake Trap"]
                                   :minions [(create-minion "Knife Juggler" :id "j")]}]) $
                    (attack $ "p1" "k" "j")
                    (get-minions $ "p2")
                    (map :name $))
              ["Knife Juggler", "Snake", "Snake", "Snake"])

         ;; If Kato attacks Knife Juggler, only Kato should be destroyed
         (is= (as-> (create-game [{:minions [(create-minion "Kato" :id "k")]}
                                  {:secrets ["Snake Trap"]
                                   :minions [(create-minion "Knife Juggler" :id "j")]}]) $
                    (attack $ "p1" "k" "j")
                    (get-minions $ "p1")
                    (map :name $))
              [])
         )