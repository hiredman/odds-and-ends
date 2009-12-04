;requires tagsoup
(defn startparse-tagsoup [s ch]
      (let [p (org.ccil.cowan.tagsoup.Parser.)]
                    (.setContentHandler p ch)
                    (.parse p s)))

(defn zip-soup [url]
      (clojure.zip/xml-zip (clojure.xml/parse url startparse-tagsoup)))


(def wishlist-url "http://secure.newegg.com/WishList/PublicWishDetail.aspx?WishListNumber")

(defn wishlist [num]
  (format "%s=%s" wishlist-url (str num)))

(defn pick [choices tag]
  ((comp first filter) #(= tag (:tag %)) choices))

(def wishlist-price
  (comp #(BigDecimal. %)
        (partial into-array Character/TYPE)
        rest first :content last
        (partial filter #(= "totalPrice" (-> % :attrs :class)))
        (partial filter #(= :td (:tag %)))
        (partial tree-seq map? (comp seq :content))
        first
        zip-soup
        wishlist))

(comment 
  user=> (wishlist-price 8795214)
  147.95M
  user=>
)
