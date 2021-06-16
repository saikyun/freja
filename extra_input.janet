(use ./freja/new_gap_buffer)

(put (gb-data :binds)
     (chr "(") (fn [gb]
                 (insert-char! gb (chr "("))
                 (insert-char! gb (chr ")"))
                 (backward-char gb)))


(key-down? :8)
