;; pronoun.is - a website for pronoun usage examples
;; Copyright (C) 2014 - 2018 Morgan Astra

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

(ns pronouns.pages
  (:require [clojure.string :as s]
            [pronouns.config :refer [pronouns-table]]
            [pronouns.util :as u]
            [hiccup.core :refer :all]
            [hiccup.element :as e]
            [hiccup.util :refer [escape-html]]))

(defn prose-comma-list
  [items]
  (let [c (count items)]
    (cond
      (<= c 1) (or (first items) "")
      (= c 2) (s/join " and " items)
      :else (str (s/join ", " (butlast items)) ", and " (last items)))))

(defn href
  [url text]
  [:a {:href url} text])

;; FIXME morgan.astra <2018-11-14 Wed>
;; use a div for this instead of a plain bold tag
(defn wrap-pronoun
  [pronoun]
  [:b pronoun])

(defn render-sentence [& content]
  [:p [:span.sentence content]])

(defn subject-example
  [subject]
  (render-sentence (wrap-pronoun (s/capitalize subject)) " foi ao parque."))

(defn object-example
  [object]
  (render-sentence "Eu fui com " (wrap-pronoun object) "."))

; (defn posessive-determiner-example
;   [subject possessive-determiner]
;   (render-sentence " O frisbee é "
;                    (wrap-pronoun possessive-determiner)
;                    ))

(defn possessive-pronoun-example
  [possessive-pronoun]
  (render-sentence "Acho que isso era "
                   (wrap-pronoun possessive-pronoun)
                   "."))

(defn reflexive-example
  [subject reflexive]
  (render-sentence (wrap-pronoun (s/capitalize subject))
                   " jogou o frisbee para "
                   (wrap-pronoun reflexive)
                   "."))

(defn header-block [header]
  [:div {:class "section title"}
   (href "/" [:h1 header])])

(defn examples-block
  [subject object posessive-determiner-example possessive-pronoun reflexive]
  (let [sub-obj (s/join "/" [subject object])
        header-str (str "Aqui estão exemplos de frases usando meus pronomes:")]
    [:div {:class "section examples"}
     [:h2 header-str]
     [:p (subject-example subject)
      (object-example object)
        ;  (posessive-determiner-example subject possessive-determiner)
      (possessive-pronoun-example possessive-pronoun)
      (reflexive-example subject reflexive)]]))

(defn usage-block []
  [:div {:class "section usage"}
   [:p "Uso completo: "
    ;; FIXME morgan.astra <2018-11-14 Wed>
    ;; This looks really ugly in the browser
    [:tt "http://www.pronom.es/pronome-sujeito/pronome-objeto/determinante-possessivo/pronome-possessivo/pronome-reflexivo"]
    " mostra exemplos do seu pronome."]])
  ;  [:p "This is a bit unwieldy. If we have a good guess we'll let you use"
  ;      " just the first one or two."]])

(defn contact-block []
  (let [twitter-name (fn [handle] (href (str "https://www.twitter.com/" handle)
                                        (str "@" handle)))]
    [:div {:class "section contact"}
     [:p "traduzido por "
      (twitter-name "__biancarosa")
      ", cujo "
      (href "https://pronom.es/ela" "pronome é ela/dela.")]
     [:p "pronom.es é um fork do "
      (href "https://pronoun.is" "pronoun.is")
      " que é feito por "
      (twitter-name "morganastra")
      ", cujo "
      (href "https://pronom.es/ela" "pronome é ela/dela")
      ". visite o projeto no "
      (href "https://github.com/biancarosa/pronom.es" "github")]
     [:p "&lt;3"]]))

(defn footer-block []
  [:footer (usage-block) (contact-block)])

(defn format-pronoun-examples
  [pronoun-declensions]
  (let [sub-objs (map #(s/join "/" (take 2 %)) pronoun-declensions)
        title (str "Ilha dos Pronomes: exemplos com " (prose-comma-list sub-objs))
        examples (map #(apply examples-block %) pronoun-declensions)]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:meta {:name "description" :content (u/strip-markup examples)}]
       [:meta {:name "twitter:card" :content "summary"}]
       [:meta {:name "twitter:title" :content title}]
       [:meta {:name "twitter:description" :content (u/strip-markup examples)}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       examples
       (footer-block)]])))

(defn table-lookup* [pronouns-string]
  (let [inputs (s/split pronouns-string #"/")
        n (count inputs)]
    (if (>= n 5)
      (take 5 inputs)
      (u/table-lookup inputs @pronouns-table))))

(defn lookup-pronouns
  "Given a seq of pronoun sets, look up each set in the pronouns table"
  [pronoun-sets]
  (->> pronoun-sets
       (map (comp table-lookup* escape-html))
       (filter some?)))

(defn make-link [path]
  (let [link (str "/" path)
        label path]
    [:li (href link label)]))

(defn front []
  (let [abbreviations (take 6 (u/abbreviate @pronouns-table))
        links (map make-link abbreviations)
        title "Ilha dos Pronomes"
        description "Pronom.es é um site de exemplos de uso de pronomes pessoais."]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "description" :content description}]
       [:meta {:name "twitter:card" :content "summary"}]
       [:meta {:name "twitter:title" :content title}]
       [:meta {:name "twitter:description" :content description}]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section table"}
        [:p "pronom.es é um site de exemplos de uso de pronomes pessoais."]
        [:p "aqui estão alguns pronomes que esse site conhece:"]
        [:ul links]
        [:p [:small (href "all-pronouns" "ver todos os pronomes")]]]]
      (footer-block)])))

(defn all-pronouns []
  (let [abbreviations (u/abbreviate @pronouns-table)
        links (map make-link abbreviations)
        title "Ilha dos Pronomes"]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section table"}
        [:p "Todos os pronomes conhecidos por esse site:"]
        [:ul links]]]
      (footer-block)])))

(defn not-found [path]
  (let [title "Ilha dos Pronomes: Exemplos em Português"
        or-re #"/[oO][rR]/"]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section examples"}
        [:p [:h2 "Não achamos esse pronome na nossa base :("]
         "Se você acha que deveriamos ter, entre em contato!!"]
        (when (re-find or-re path)
          (let [alts (s/split path or-re)
                new-path (str "/" (s/join "/:OR/" alts))]
            [:div
             "Você quis dizer: "
             (href new-path
                   (str "pronom.es"
                        new-path))]))]
       (footer-block)]])))

(defn pronouns [params]
  (let [path (params :*)
        param-alts (u/vec-coerce (or (params "or") []))
        path-alts (s/split path #"/:[oO][rR]/")
        pronouns (lookup-pronouns (concat path-alts param-alts))]
    (if (seq pronouns)
      (format-pronoun-examples pronouns)
      (not-found path))))
