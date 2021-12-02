;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HtDF Design Quiz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Image, Image -> Boolean
;; Returns true if first image's area is bigger than second's
(check-expect (larger? (square 10 "outline" "red") (square 40 "outline" "red")) false)
(check-expect (larger? (square 40 "outline" "red") (square 40 "outline" "red")) false)
(check-expect (larger? (square 50 "outline" "red") (square 40 "outline" "red")) true)
(check-expect (larger? (square 0 "outline" "red") (square 40 "outline" "red")) false)
(check-expect (larger? (rectangle 40 40 "outline" "red") (square 40 "outline" "red")) false)
(check-expect (larger? (rectangle 50 40 "outline" "red") (square 40 "outline" "red")) true)

;(define (larger? img1 img2) True) ;stub

;(define (larger? img1 img2)       ;template
; (...img1 img2)

(define (larger? img1 img2)
  (>
   (* (image-width img1)
      (image-height img1))
   (* (image-width img2)
      (image-height img2))))