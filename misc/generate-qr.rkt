#lang racket

(require simple-qr)
(define my-addy "http://192.168.200.100:8080/")
;(define my-addy "google.com")
(qr-write my-addy "normal.png")
(qr-write my-addy "small.png" #:module_width 2)
(qr-write my-addy "large.png" #:module_width 10)
