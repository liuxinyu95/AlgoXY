;; Trie utilities

(define (list->trie lst) ;;lst is list of pairs
  (define (insert-pair t p)
    (insert t (car p) (cadr p)))
  (fold-left insert-pair '() lst))