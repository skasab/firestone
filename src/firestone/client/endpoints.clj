(ns firestone.client.endpoints
  (:require [clojure.string :refer [starts-with?]]
            [clojure.data.json :refer [read-json write-str]]
            [firestone.client.edn-api :refer [attack!
                                              create-game!
                                              end-turn!
                                              play-minion-card!
                                              play-spell-card!
                                              redo!
                                              undo!
                                              use-hero-power!]]))

(defn create-response
  [response]
  {:status  200
   :headers {"Content-Type"                 "text/json; charset=utf-8"
             "Access-Control-Allow-Origin"  "*"
             "Access-Control-Allow-Methods" "*"}
   :body    (write-str response)})

(defn handler [request]

  (let [uri (:uri request)]

    (cond (starts-with? uri "/createGame")
          (time (create-response (create-game!)))

          (starts-with? uri "/endTurn")
          (let [params (read-json (slurp (:body request)))]
            (create-response (end-turn!)))

          (starts-with? uri "/attack")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)
                attacker-id (:attackerId params)
                target-id (:targetId params)]
            (create-response (attack! player-id attacker-id target-id)))

          (starts-with? uri "/playMinionCard")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)
                card-id (:cardId params)
                position (:position params)
                target-id (:targetId params)]
            (create-response (play-minion-card! player-id card-id target-id)))

          (starts-with? uri "/playSpellCard")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)
                card-id (:cardId params)
                target-id (:targetId params)]
            (create-response (play-spell-card! player-id card-id target-id)))

          (starts-with? uri "/useHeroPower")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)
                target-id (:targetId params)]
            (create-response (use-hero-power! player-id target-id)))

          (starts-with? uri "/undo")
          (create-response (undo!))

          (starts-with? uri "/redo")
          (create-response (redo!))

          )))

