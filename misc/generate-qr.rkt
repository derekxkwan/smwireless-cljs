#lang racket

(require simple-qr)
(define my-addy "http://169.254.163.59:3000")
(qr-write my-addy "normal.png")
(qr-write my-addy "small.png" #:module_width 2)
(qr-write my-addy "large.png" #:module_width 10)
