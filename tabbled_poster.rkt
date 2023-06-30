#lang racket/gui

(require racket/draw)

(define poster-width 842)     ; Width of the main poster
(define poster-height 595)    ; Height of the main poster
(define tab-width 30)         ; Width of each tear-off tab
(define tab-height 60)        ; Height of each tear-off tab
(define tab-gap 10)           ; Gap between each tear-off tab

(define frame (new frame%
                   [label "Tabbed Poster Generator"]
                   [width (+ poster-width 50)]
                   [height (+ poster-height 150)]))

(define main-text-field (new text-field% [parent frame] [label "Main Poster Text"] [init-value "Take 1!"]))
(define tab-text-field (new text-field% [parent frame] [label "Tab Texts (comma separated)"]))

(define button (new button% [parent frame] [label "Generate"]
                    [callback (lambda (button event)
                     (generate-tabbed-poster event))]))

(define panel (new vertical-panel% [parent frame]))

;(send panel add main-text-field)
;(send panel add tab-text-field)
;(send panel add button)

(define (create-tabbed-poster main-text tab-text)
  (define img (make-bitmap poster-width poster-height))
  (define dc (new bitmap-dc% [bitmap img]))
  (send dc set-background "white")
  (send dc clear)
  
  ;; Draw the main poster
  (send dc set-text-foreground "black")
  (send dc set-font (make-object font% 36 'roman 'normal 'bold))
  (send dc draw-text main-text 20 20)
  
  ;; Draw tear-off tabs
  (define tab-x-start (+ tab-gap tab-width))
  (define tab-y-start (- poster-height tab-height tab-gap))
  
  (send dc set-text-foreground "black")
  (send dc set-font (make-object font% 12 'roman 'normal 'normal))
  
  (define tab-count (exact-floor (/ (- poster-width (* 2 tab-gap)) (+ tab-width tab-gap))))
  
  ;; Draw dotted line around tabs
  (send dc set-pen "black" 1 'dot)
   ;; Set brush color to transparent
  (define transparent-color (make-object color% 0 0 0 0))  ; Alpha value of 0 for transparency
  (send dc set-brush (new brush% [style 'transparent] [color transparent-color]))

  
  (define (draw-tab x y)
    (send dc draw-text tab-text x y #t 0 (/ pi 2))
    (send dc draw-rectangle (- x 5) (- y 240) (+ tab-width 10) 400))  
  
  (for ([i (in-range tab-count)])
    (let* ([tab-x (+ tab-x-start (* i (+ tab-width tab-gap)))]
           [tab-y tab-y-start])
      (draw-tab tab-x tab-y)))
  
  (send dc get-bitmap))

(define (convert-images-to-pdf images output-pdf)
  (system "convert output/*.png -rotate 90  -background white -page a4 myoutput.pdf"))

(define (generate-tabbed-poster event)
  (define main-text (send main-text-field get-value))
  (define tab-text (send tab-text-field get-value))
  (define tab-texts (string-split tab-text ","))  ; Split tab-text on commas

  ;; Generate tabbed posters for each tab text
  (define output-dir "output")  ; Directory to store individual tabbed poster images
  (make-directory* output-dir)
  
    ;; Generate tabbed posters for each tab text
  (for-each
   (lambda (text)
     (define tabbed-poster (create-tabbed-poster main-text text))
     
     ;; Save the image to a file with tab text as the name
     (send tabbed-poster save-file (format "~a/tabbed_poster_~a.png" output-dir text) 'png))
   tab-texts)

   ;; Convert images to PDF
  (define output-pdf "tabbed_posters.pdf")
  (define images (map path->string (directory-list output-dir)))
  
  (convert-images-to-pdf (map (lambda (image) (string-append output-dir "/" image)) images) output-pdf)
  
  ;; Remove individual image files
  (for-each
   (lambda (image)
     (delete-file (string-append output-dir "/" image)))
   images)

  (message-box "Generation Complete" "Tabbed posters have been generated as a pdf."))


(send frame show #t)