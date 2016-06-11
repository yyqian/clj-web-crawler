(ns clj-web-crawler.core
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]
            [clojure.string :as str]))

(defn http-get
  [url]
  (client/get url
              {:headers {:Accept          "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
                         :Accept-Language "en-US,en;q=0.8"
                         :User-Agent      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.84 Safari/537.36"}}))

(defn leetcode
  "leetcode crawler"
  []
  (let [url "https://leetcode.com/problemset/algorithms/"
        resource (-> url
                     http-get
                     :body
                     java.io.StringReader.
                     html/html-resource)
        problem-set (->> (html/select resource [:tbody :tr])
                         (map #(let [id (-> %
                                            :content
                                            (nth 3)
                                            :content
                                            first)
                                     title (-> %
                                               :content
                                               (nth 5)
                                               :content
                                               second
                                               :content
                                               first)
                                     link (-> %
                                              :content
                                              (nth 5)
                                              :content
                                              second
                                              :attrs
                                              :href)
                                     lock (let [content (-> %
                                                            :content
                                                            (nth 5)
                                                            :content)]
                                            (> (count content) 3))]
                                {:id    id
                                 :title title
                                 :link  (str "https://leetcode.com" link)
                                 :lock  lock})))
        free-problem-set (->> problem-set
                              (filter #(not (:lock %)))
                              (map #(hash-map :id (:id %) :title (:title %) :link (:link %))))]
    free-problem-set))

(defn smzdm
  "smzdm.com crawler"
  [page]
  (let [url (str "http://www.smzdm.com/p" page)
        resource (-> url
                     http-get
                     :body
                     java.io.StringReader.
                     html/html-resource)
        lines (->> (html/select resource [:div.leftWrap :> :div.list])
                   rest
                   rest
                   (filter #(-> %
                                :content
                                (nth 5)
                                :content
                                (nth 5)
                                :content
                                count
                                (= 13))))
        items (->> lines
                   (map (fn [line]
                          (let [lr-bot (-> line
                                           :content
                                           (nth 5)
                                           :content
                                           (nth 5)
                                           :content)
                                good (-> lr-bot
                                         (nth 1)
                                         :content
                                         second
                                         :content
                                         (nth 2)
                                         :content
                                         first
                                         read-string)
                                bad (-> lr-bot
                                        (nth 3)
                                        :content
                                        second
                                        :content
                                        (nth 2)
                                        :content
                                        first
                                        read-string)
                                fav (-> lr-bot
                                        (nth 5)
                                        :content
                                        second
                                        :content
                                        first
                                        read-string)
                                comment (-> lr-bot
                                            (nth 7)
                                            :content
                                            second
                                            :content
                                            first
                                            read-string)
                                h4-a (-> line
                                         :content
                                         second
                                         :content
                                         (nth 3)
                                         :content
                                         second)
                                link (-> h4-a
                                         :attrs
                                         :href)
                                title (-> h4-a
                                          :content
                                          first
                                          (str/replace #"\n" "")
                                          str/trim)
                                price (-> h4-a
                                          :content
                                          second
                                          :content
                                          first)
                                pic (-> line
                                        :content
                                        (nth 3)
                                        :content
                                        second
                                        :attrs
                                        :src)]
                            {:good good :bad bad :link link :title title :price price :fav fav :comment comment :pic pic}))))]
    items))

(defn smzdm-n
  [n]
  (if (> n 1)
    (->> (range 1 n)
         (map #(smzdm %))
         (reduce into []))
    (smzdm 1)))

(->> (smzdm-n 3)
     (filter #(and (> (:good %) (* (:bad %) 5))
                   (> (:good %) 50)
                   (> (:fav %) 10)
                   (> (:comment %) 10)))
     (map #(str (:title %)
                " | "
                (:price %)
                " | "
                (int (/ (* (:good %) 100.0) (+ (:good %) (:bad %))))
                "% | "
                (+ (:good %) (:bad %) (:fav %) (:comment %))))
     (map #(println %))
     (reduce =))

(->> (smzdm-n 1)
     (filter #(and (> (:good %) (* (:bad %) 5))
                   (> (:good %) 50)
                   (> (:fav %) 10)
                   (> (:comment %) 10)))
     (take 3))