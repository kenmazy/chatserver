(ns echoserver.core
  (:require [server.socket])
  (:import [java.io BufferedReader InputStreamReader PrintWriter])
  (:gen-class))

(def user-list (ref []))
(defn add-user [out]
  (dosync
    (alter user-list conj out)))

(defn print-to-all [line selfout]
  (doseq [user (filter #(not= % selfout) @user-list)]
    (binding [*out* user] (println line))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (server.socket/create-server 9999
    (fn [in out]
      (binding [*in* (BufferedReader. (InputStreamReader. in))
                *out* (PrintWriter. out)]
        (add-user *out*)
        (loop []
          (print-to-all (read-line) *out*)
          (recur))))))
