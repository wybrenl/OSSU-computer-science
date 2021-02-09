;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Booleans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;true
;false

(define WIDTH 100)
(define HEIGHT 100)

;(> WIDTH HEIGHT)
;(>= WIDTH HEIGHT)
;(= 1 2)
;(= 1 1)
;(> 3 9)

;(string=? "foo" "bar")

(define r1 (rectangle 10 20 "solid" "red"))
(define r2 (rectangle 20 10 "solid" "blue"))
;(= (image-width r1) (image-width r2))

;(if (< (image-width r2)
;       (image-height r2))
;    (image-width r2)
;    (image-height r2))

(and (> (image-height r1) (image-height r2))
     (< (image-width r1) (image-width r2)))

;(or (
;(not (
    