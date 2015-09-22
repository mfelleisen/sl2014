#lang racket

;; N -> Contract
(define (string-of-length n)
  (flat-named-contract
   (format "string of length ~a" n)
   (and/c string? (compose (>=/c n) string-length))))

(provide
 LOCALHOST
 (contract-out
  (speak
   ;; (speak HW IP)
   ;; a universe client displays HW, connects to IP, and returns one message
   ;; effect: it sends a message when the player hits RETURN
   ;; effect: it shuts down when it receives the first message
   (-> (string-of-length 10) string? string?))))

;; ---------------------------------------------------------------------------

(require 2htdp/image 2htdp/universe)

;; type Chat = String

;; Chat String String -> Chat 
(define (speak s HOST)
  ;; --- IN --- 
  (big-bang s
    [register HOST]
    [on-receive (lambda (w m) (stop-with m))]
    [on-key edit]
    [on-mouse (lambda (world-state x y m) "")]
    [to-draw display]))

;; Chat -> Image 
(define (display s)
  (text s 200 'purple))

;; Chat Key -> (U Chat [Package Chat String])
(define (edit world-state key)
  (cond
    [(key=? "\r" key) (make-package world-state world-state)]
    [(key=? "shift" key) world-state]
    [(key=? "\b" key) (delete-last world-state)]
    [else (string-append world-state key)]))

;; Chat -> Chat
(define (delete-last s)
  (if (string=? s "")
      ""
      (substring s 0 (- (string-length s) 1))))
