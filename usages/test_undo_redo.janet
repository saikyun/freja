(import ../freja/new_gap_buffer :prefix "")
(import ../freja/new_gap_buffer_util :prefix "")

(comment
  (-> (string->gb "abcd|")
      (delete-region! 0 2)
      undo!
      render)
  #=> "abcd|"
  )
