(ns clojure.playground-zero
  (:require [zeromq.zmq :as zmq]
            [zeromq.sendable :as zs]
            [clojure.core.async :refer [>! <! >!! <!! chan put! take! timeout close! dropping-buffer go]]))

;;; nb...
#_((def ^:const socket-types
  {:pair (ZMQ/PAIR)
   :pub (ZMQ/PUB)
   :sub (ZMQ/SUB)
   :req (ZMQ/REQ)
   :rep (ZMQ/REP)
   :xreq (ZMQ/XREQ)
   :xrep (ZMQ/XREP)
   :dealer (ZMQ/DEALER)
   :router (ZMQ/ROUTER)
   :xpub (ZMQ/XPUB)
   :xsub (ZMQ/XSUB)
   :pull (ZMQ/PULL)
   :push (ZMQ/PUSH)})
)


;; No hygiene here!
(defn zmq-chan [context socket-type addr channel-push-or-pull bind-or-connect]
  (let [zfn    (case bind-or-connect :bind zmq/bind :connect zmq/connect)
        skt    (-> (zmq/socket context channel-push-or-pull)
                  (zfn addr))
        c     (chan (dropping-buffer 137))]
    (case channel-push-or-pull
      :push (go (loop [] (let [^String msg (<! c)]
                           (if (nil? msg)
                             (zmq/close skt)
                             (do 
                               (zs/send msg skt 0)
                               (recur))))))
      ; TODO: how/when to close socket
      :pull (go (while true (let [msg (String. (zmq/receive-all skt))]
                              (println "Got" msg "from" skt)
                              (>! c msg)))))
    c))



(defn zmq-chan2 [context socket-type addr channel-push-or-pull bind-or-connect]
  (let [zfn    (case bind-or-connect :bind zmq/bind :connect zmq/connect)
        skt    (-> (zmq/socket context channel-push-or-pull)
                  (zfn addr))
        c     (chan (dropping-buffer 137))]
    (case channel-push-or-pull
      :push (future (loop [] (let [^String msg (<!! c)]
                           (if (nil? msg)
                             (zmq/close skt)
                             (do 
                               (zs/send msg skt 0)
                               (recur))))))
      ; TODO: how/when to close socket
      :pull (future (while true (let [msg (String. (zmq/receive-all skt))]
                              (println "Got" msg "from" skt)
                              (>!! c msg)))))
    c))



#_(

   (def context (zmq/zcontext))

   (def pull (zmq-chan ctx :pull "tcp://*:12348" :pull :bind))

   (def push (zmq-chan ctx :push "tcp://127.0.0.1:12348" :push :connect))

    (deftest push-pull-test
      (with-open [push (-> (zmq/socket context :push)
                           (zmq/connect "tcp://127.0.0.1:12349"))
                  pull (-> (zmq/socket context :pull)
                           (zmq/bind "tcp://*:12349"))]
        (zs/send "hello" push 0)
        (let [actual (String. (zmq/receive pull))]
          (is (= "hello" actual)))))

    (with-open [push (-> (zmq/socket context :push)
                           (zmq/connect "tcp://127.0.0.1:12349"))
                  pull (-> (zmq/socket context :pull)
                           (zmq/bind "tcp://*:12349"))]
        (zs/send "hello" push 0)
        (println (String. (zmq/receive pull))))


    (deftest receive-str-timeout-test
      (with-open [pull (-> (zmq/socket context :pull)
                           (zmq/bind "tcp://*:12310"))]
        (zmq/set-receive-timeout pull 100)
        (let [actual (zmq/receive-str pull)]
          (is (= nil actual)))))
)
