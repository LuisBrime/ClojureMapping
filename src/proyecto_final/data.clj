(ns proyecto-final.data)

; use pretty print to see it all nice and beauty
(use 'clojure.pprint)

; CSV file with the database
(def file "resources/dato2.csv")

; --- BEAUTIFY FUNCTIONS ---

; all the keys for the data set
(def vg-keys [:platform :genre :publisher :na_sales :eu_sales :jp_sales :other_sales :global_sales :rating :critic_score])

; function to pass from string to integer with decimals
(defn str->int
  [x]
  (Float. x)
  )

; all keys paired with their conversion function if necessary
(def tuples {
             :platform     identity
             :genre        identity
             :publisher    identity
             :na_sales     str->int
             :eu_sales     str->int
             :jp_sales     str->int
             :other_sales  str->int
             :global_sales str->int
             :rating       identity
             :critic_score identity
             })

; function to convert a value if necessary
(defn convert
  [key value]
  ((get tuples key) value)
  )

; parse data to a seq of strings
(defn parsedata
  [x]
  (map #(clojure.string/split %1 #",") (clojure.string/split x #"\n"))
  )

; convert the seq of strings into a seq of maps
(defn mapify
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [key value]]
                   (assoc row-map key (convert key value)))
                 {}
                 (map vector vg-keys unmapped-row)))
       rows))

; final data set ready to use
(def vgdataset (mapify (parsedata (slurp file))))

(defn load
    []
    (as-> (mapify (parsedara (slurp file))))
    )

; --- AUXILIAR FUNCTIONS ---

(defn more
  [x]
  (> x 10)
  )

; --- FUNCTIONS TO GET DATA OUT OF THE DATA SET ---
(defn by-publisher
  "Separate games sales by the publisher"
  [dataset publisher]
  (filter #(= 0 (compare (get %1 :publisher) publisher)) dataset)
  )

(defn by-genre
  "Separate games sales by genre"
  [dataset genre]
  (filter #(= 0 (compare (get %1 :genre) genre)) dataset)
  )

(defn by-critic
  [dataset critic]
  (filter #(= 0 (compare (get %1 :critic_score) critic)) dataset)
  )

(defn sales
  "Separate games if their sales number in a region is higher than 10"
  [dataset where]
  (cond
    (= 0 (compare where "NA")) (filter #(more (get %1 :na_sales)) dataset)
    (= 0 (compare where "EU")) (filter #(more (get %1 :eu_sales)) dataset)
    (= 0 (compare where "JP")) (filter #(more (get %1 :jp_sales)) dataset)
    (= 0 (compare where "GL")) (filter #(more (get %1 :global_sales)) dataset)
    (= 0 (compare where "OT")) (filter #(more (get %1 :other_sales)) dataset)
    :otherwise (println "Enter valid region.")
    )
  )


; --- TESTS ---
;(pprint vgdataset)

;(def nintendoset (by-publisher vgdataset "Nintendo"))
;(pprint nintendoset)

;(def sportsset (by-genre vgdataset "Sports"))
;(pprint sportsset)

;(def more_sales (sales vgdataset "EU"))
;(pprint more_sales)

;(def excelentcritic (by-critic vgdataset "Excelente"))
;(pprint excelentcritic)


