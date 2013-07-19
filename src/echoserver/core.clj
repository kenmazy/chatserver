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

(defn delete-room! [roomname]
  (let [room (@room-list roomname)]
    (if (empty? @room) 
      (swap! room-list dissoc roomname))))
         
(defn rand-username []
  (str "Guest" (rand-int 10000)))

(defn in-room? [room username]
  (let [out (room username)]
    (and out (= out *out*))))

(defn agent-add-user [room username out]
  (if-not (room username)
    (assoc room username out)
    room))

(defn agent-say [room msg]
  (doseq [[_ out] room]
    (binding [*out* out] (println msg)))
  room)

(defn agent-remove-user [room username]
  (dissoc room username))

(defn agent-swap-username [room username new-username]
  (if-not (room new-username)
    (let [out (room username)]
      (assoc (dissoc room username) new-username out))
  room))

(defn add-user [roomname username]
  (let [room (add-get-room! roomname)]
    (loop [curr-username username]
      (send room agent-add-user curr-username *out*)
      (await room)
      (if (in-room? @room curr-username)
        (list roomname curr-username)
        (recur (rand-username))))))
          
(defn remove-user [roomname username]
  (let [room (@room-list roomname)]
    (if (and room (in-room? @room username))
      (do
        (send room agent-remove-user username)
        (await room)
        (send room agent-say (str username " has left the room."))
        (delete-room! roomname)))))

(defn say [roomname username msg]
  (send (@room-list roomname) agent-say (str username ": " msg)))

(defn swap-username [roomname username new-username] 
  (let [room (@room-list roomname)]
    (send room agent-swap-username username new-username)
    (await room)
    (if (in-room? @room username)
      (do
        (println "Nick commmand failed, username taken.")
        (list roomname username))
      (do
        (send room agent-say (str username " is now known as " new-username))
        (list roomname new-username)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (server.socket/create-server 9999
    (fn [in out]
      (binding [ *in* (BufferedReader. (InputStreamReader. in))
                *out* (PrintWriter. out)]
          (loop [[roomname username] ["Default" (rand-username)] input "/join Default"]
            (let [[cmd & args] (clojure.string/split input #" ")]
              (case cmd
                "/join" (do
                          (remove-user roomname username)
                          (recur (add-user (first args) username) (read-line)))
                "/nick" (recur (swap-username roomname username (first args)) (read-line))
                "/exit" (remove-user roomname username)
                (do
                  (say roomname username input)
                  (recur [roomname username] (read-line))))))))))

