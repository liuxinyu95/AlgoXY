;; Trie utilities

(define (list->trie lst) ;;lst is list of pairs
  (define (insert-pair t p)
    (insert t (car p) (cadr p)))
  (fold-left insert-pair '() lst))

;; filter is defined in R6RS, but not in R5RS
(define (filter pred lst)
  (keep-matching-items lst pred))