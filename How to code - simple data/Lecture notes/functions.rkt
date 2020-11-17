;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; function-definitions-starter.rkt

;(above(circle 10 "solid" "red")
;      (circle 10 "solid" "black")
;      (circle 10 "solid" "green"))

(define (bulb c)
  (circle 10 "solid" c))

(beside (bulb "purple")
        (bulb "yellow")
        (bulb "pink"))

(bulb (string-append "re" "d"))



  