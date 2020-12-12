(def state
  @{:x 0 :y 0})

(def rows
  [{:y 10
    :words ["hello" " " " " "mr cat"]
	:buffer @"hello  mr cat"
	:xs    [0 5 10 15 (comment etc)]}])

(defn pos-in-text
  [rows pos]
  
