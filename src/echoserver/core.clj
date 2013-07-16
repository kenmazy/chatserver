(ns echoserver.core
  (:require [server.socket])
  (:import [java.io BufferedReader InputStreamReader PrintWriter])
  (:gen-class))

(def room-list (atom {}))
(def ^:dynamic *username*)
(def ^:dynamic *room*)

(defn add-get-room! [roomname]
  (or (@room-list roomname)
    (let [room (agent {})]
      (swap! room-list assoc roomname room)
      room)))

(defn delete-empty-rooms! []
  (swap! room-list
        #(apply dissoc % (for [[k v] % :when (empty? @v)] k))))
         

(defn agent-add-user-to-room [room username out]
  (if (not (room username))
    (assoc room username out)
    room))

(defn agent-remove-user-from-room [room username]
  (dissoc room username))
  
(defn agent-print-to-room [room msg]
  (doseq [[_ out] room]
    (binding [*out* out] (println msg)))
  room)

(defn agent-swap-username [room old-username new-username]
  (if (not (room new-username))
    (let [out (room old-username)]
      (assoc (dissoc room old-username) new-username out))
    room))


(defn add-user-to-room []
  (send *room* agent-add-user-to-room *username* *out*))

(defn remove-user-from-room []
  (send *room* agent-remove-user-from-room *username*)
  (if (empty? @*room*) (delete-empty-rooms!)))

(defn say-to-room [msg]
  (send *room* agent-print-to-room (str *username* ": " msg)))

(defn announce-join-to-room []
  (if *room* (send *room* agent-print-to-room (str *username* " has joined the room."))))

(defn announce-leave-to-room []
  (if *room* (send *room* agent-print-to-room (str *username* " has left the room."))))

(defn announce-change-username-to-room [new-username]
  (send *room* agent-print-to-room (str *username* " is now known as " new-username ".")))

(defn swap-username [new-username] 
  (send *room* agent-swap-username *username* new-username))

(defn rand-username []
  (str "Guest" (rand-int 10000)))

(defn in-room? []
  (let [user (@*room* *username*)]
    (and user (= *out* user))))

(defn clean-exit []
  (remove-user-from-room)
  (announce-leave-to-room))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (server.socket/create-server 9999
    (fn [in out]
      (binding [*username* "Guest"
                *room* nil
                *in* (BufferedReader. (InputStreamReader. in))
                *out* (PrintWriter. out)]
        (try
          (loop [line "/join Default"]
            (if line
              (let [[cmd & args] (clojure.string/split line #" ")]
                (case cmd
                  "/join" (do
                            (if *room* 
                              (do
                                (remove-user-from-room)
                                (announce-leave-to-room)))
                            (set! *room* (add-get-room! (first args)))
                            (add-user-to-room)
                            (await *room*)
                            (while (not (in-room?))
                              (set! *username* (rand-username))
                              (add-user-to-room)
                              (await *room*))
                            (announce-join-to-room)
                            (recur (read-line)))
                  "/nick" (let [new-username (first args)]
                            (swap-username new-username)
                            (await *room*)
                            (if (not (in-room?))
                              (do
                                (announce-change-username-to-room new-username)
                                (set! *username* new-username))
                              (println "nick command failed, username taken"))
                            (recur (read-line)))
                  "/exit"   :done
                  (do
                    (say-to-room line)
                    (recur (read-line)))))))
          (finally (clean-exit)))))))
