(ns samples.scraping.scraping
  (:require
   [net.cgrand.enlive-html :as html]))

(defn hn-titles []
  ;; parse the html into an enlive data structure
  (-> (slurp "https://news.ycombinator.com/")
      java.io.StringReader.
      html/html-resource
      
      ;; grab 'td.title > span.titleline > a]'
      (html/select [:td.title :span.titleline :> :a html/text-node])))

(defn extract-multiple-values []
  (let [resource (-> (slurp "https://news.ycombinator.com/")
                     java.io.StringReader.
                     html/html-resource)
        title-nodes (html/select resource [:td.title :span.titleline :> :a]) ;; nodes for title & href
        points-text (html/select resource [:span.score html/text-node])] ;; adjacent rows with points
      
    (map (fn [title-node points-str]
           {:title (html/text title-node)
            :url (-> title-node :attrs :href)
            :points (->  (re-matches #"([0-9]+).*" points-str) last Integer/parseInt)})
         title-nodes points-text)))

(def sample-html-fragment
  "<ul>
<li class=\"foo\">Option 1</li>
<li>Option 2</li>
<li class=\"foo bar\">Option 3</li>
</ul>")

(defn negate-a-selector []
  (->  (html/html-resource (java.io.StringReader. sample-html-fragment))
       ;; not use of second [] for :li and (not= (:class %) "foo")
       (html/select [[:li (html/but (html/attr-has :class "foo"))] html/text ])
       ;; returns ("Option 2"))
       ))


(defn custom-selector []
  (let [res (html/html-resource (java.io.StringReader. sample-html-fragment))
        ;; function gets passed each node
        selector (html/pred #(and
                              (re-matches #".*3$" (html/text %)) ;; text ends in "3"
                              (contains? (html/attr-values % :class) "bar")))] ;; has class bar
    (html/select res [[:li selector]])))
