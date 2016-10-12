(ns clj-web-crawler.hacker-news
  (:require [cheshire.core :as cheshire]
            [clj-http.client :as client]
            [net.cgrand.enlive-html :as html])
  (:gen-class)
  (:import (java.io StringReader)))

(defn story-ids
  [category]
  (read-string (slurp (str "https://hacker-news.firebaseio.com/v0/" category ".json"))))

(defn beststory-ids
  []
  (story-ids "beststories"))

(defn item-url
  [id]
  (str "https://hacker-news.firebaseio.com/v0/item/" id ".json"))

(defn beststories
  [n]
  (->> (beststory-ids)
       (map item-url)
       (map slurp)
       (map cheshire/parse-string)
       (take n)))

(defn get-login-tokens
  []
  (let [raw (-> "http://imagine.yyqian.com/login"
                client/get)
        resource (-> raw
                     :body
                     StringReader.
                     html/html-resource)]
    {:_csrf   (->> (html/select resource [:table :input])
                   (filter #(= "_csrf" (get-in % [:attrs :name])))
                   first
                   :attrs
                   :value)
     :cookies (:cookies raw)}))

(defn get-pass-cookies
  []
  (let [tokens (get-login-tokens)]
    (:cookies (client/post "http://imagine.yyqian.com/login"
                           {:form-params {:username "yyqian"
                                          :password "Qyy870121"
                                          :_csrf    (:_csrf tokens)}
                            :cookies     (:cookies tokens)}))))

(defn get-submit-tokens
  []
  (let [pass-cookies (get-pass-cookies)]
    {:cookies pass-cookies
     :_csrf   (->> (-> "http://imagine.yyqian.com/post/new/editor"
                       (client/get {:cookies pass-cookies})
                       :body
                       StringReader.
                       html/html-resource
                       (html/select [:head :meta]))
                   (filter #(= "_csrf" (get-in % [:attrs :name])))
                   first
                   :attrs
                   :content)}))

(defn submit-post
  [story tokens]
  (let [form (if (nil? (story :text))
               {:title (story :title)
                :url   (story :url)
                :_csrf (tokens :_csrf)}
               {:title (story :title)
                :text  (story :text)
                :_csrf (tokens :_csrf)})]
    (client/post "http://imagine.yyqian.com/post"
                 {:form-params form
                  :cookies     (tokens :cookies)})))

(defn transfer
  [n]
  (let [submit-tokens (get-submit-tokens)]
    (doseq [story (map (fn [story]
                         {:title (get story "title")
                          :url   (get story "url")
                          :text  (get story "text")})
                       (beststories n))]
      (submit-post story submit-tokens))))
