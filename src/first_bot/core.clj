(ns first-bot.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :as a]
            [clojure.string :as str]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]
            [jsonista.core :as j]
            [clj-http.lite.client :as client])
  (:gen-class))


(def config (edn/read-string (slurp "config.edn")))

(def state (atom nil))

(def bot-id (atom nil))


(defn make-story [id]
  (format "https://hacker-news.firebaseio.com/v0/item/%d.json" id))

(defn make-link [story]
  ;((juxt :title :url)
   (-> (:body (client/get story {:accept :json}))
       (j/read-value j/keyword-keys-object-mapper)
       (:url)))

(defn top-10 []
  (let [hn-top "https://hacker-news.firebaseio.com/v0/topstories.json"]
    (map make-link
         (map make-story
              (take 5 (j/read-value (:body (client/get hn-top {:accept :json}))))))))

(defn weather [evt]
  (let [zip (or (-> evt
                  (str/split #" ")
                  (second))
                "12120")
        weather-url "https://api.openweathermap.org/data/2.5/weather"
        weather-data (-> (:body (client/get weather-url {:accept :json 
                                                         :query-params {"zip" (str zip ",th")
                                                                        "appid" (:openweathermap-app-id config)
                                                                        "units" "metric"
                                                                        "lang" "th"}}))
                         (j/read-value j/keyword-keys-object-mapper))
        desc (get-in weather-data [:weather 0 :description])
        temp (get-in weather-data [:main :temp])
        city (get-in weather-data [:name])
        sunset (-> (+ (get-in weather-data [:timezone]) (get-in weather-data [:sys :sunset]))
                   (java.time.Instant/ofEpochSecond)
                   (.toString)
                   (clojure.instant/read-instant-date))]
    (str city " " desc " อุณหภูมิ " temp " เซลเซียส, " (format "พระอาทิตย์ตกเวลา %tH:%tM" sunset sunset))))

(defn gem-forest []
  (let [gem-forest-url "https://gemforestcoffee.page365.net/products.json"
        products (-> (:body (client/get gem-forest-url))
                     (j/read-value j/keyword-keys-object-mapper))
        items (:items products)
        name-price (map (juxt :name :price) (map #(select-keys % [:name :price]) items))]
  (apply str (map (fn [[name price]] (str name " ราคา " price " บาท\n")) name-price))))

(defn random-kitty []
  (let [img-url (->
                 (:body (client/get "https://api.thecatapi.com/v1/images/search"))
                 (j/read-value j/keyword-keys-object-mapper))]
    (get-in img-url [0 :url])))


(defmulti handle-event (fn [type _data] type))

(defn random-response [user]
  (str (rand-nth (:responses config)) ", " (mention-user user) \!))

(defmethod handle-event :message-create
  [_ {:keys [channel-id author mentions] :as _data}]
  (when (some #{@bot-id} (map :id mentions))
    (discord-rest/create-message! (:rest @state) channel-id :content (random-response author))))

(defmethod handle-event :ready
  [_ _]
  (discord-ws/status-update! (:gateway @state) :activity (discord-ws/create-activity :name (:playing config))))

(defmethod handle-event :default [_ _])

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (try
    ; (message-pump! (:events @state) handle-event)
    (loop []
      (let [[event-type event-data] (a/<!! (:events @state))
            channel-id (:channel-id event-data)
            bot (:bot event-data)]
        (println "🎉 NEW EVENT! 🎉")
        (println "Event type:" event-type)
        (println "Event data:" (pr-str event-data))
        (when (and
               (= event-type :message-create)
               (not bot))
          (cond
            (= (:content event-data) "!hn")
            (discord-rest/create-message! (:rest @state) channel-id :content (str/join "\n" (top-10)))
            (= (:content event-data) "!gem")
            (discord-rest/create-message! (:rest @state) channel-id :content (gem-forest))
            (= (:content event-data) "!kitty")
            (discord-rest/create-message! (:rest @state) channel-id :content (random-kitty))
            (str/starts-with? (:content event-data) "!weather")
            (discord-rest/create-message! (:rest @state) channel-id :content (weather (:content event-data)))))
        (recur)))
    (finally (stop-bot! @state))))

