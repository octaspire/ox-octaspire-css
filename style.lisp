(ql:quickload :lass)
(ql:quickload :alexandria)
(ql:quickload :uiop)

(defun write-prefix (stream &key (one-line nil))
  (let ((text (uiop:read-file-string "LICENSE")))
    (format stream (if one-line "/*~A*/" "/*~%~A*/~%~%")
            (if one-line
                (remove #\linefeed text)
                text))))

(defun write-org-prefix (stream)
  (format stream "~A~%~A~%#+HTML_HEAD:   "
          "#+OPTIONS: html-style:nil"
          "#+HTML_HEAD: <style>"))

(defun write-org-postfix (stream)
  (format stream "~%#+HTML_HEAD: </style>"))


(defun file-as-base64 (path)
  (cl-base64:usb8-array-to-base64-string (alexandria:read-file-into-byte-vector path)))

(defparameter *css* (list '(@font-face
                            :font-family "'IBM Plex Sans'"
                            :src "external/IBM/Plex/TrueType/Sans/IBMPlexSans-Regular.ttf")
                          '(@font-face
                            :font-family "'IBM Plex Mono'"
                            :src "external/IBM/Plex/TrueType/Mono/IBMPlexMono-Regular.ttf")
                          '(body
                            :font-family "'IBM Plex Sans'" "sans-serif"
                            :background-color "#ffffea"
                            :color "black")
                          '(.toc-toggle-button
                            :font-family "'IBM Plex Mono'" monospace
                            :margin 0px)
                          '((:or nav \#footnotes \#postamble \#preamble)
                            :max-width 60ch
                            :line-height 1.4
                            :display block
                            :margin-left auto
                            :margin-right auto)
                          '(nav
                            :border-top 0.25em solid black
                            :border-bottom 0.25em solid black)
                          '(\#footnotes
                            :margin-top 2em
                            :border-top 0.25em solid black)
                          '(.footdef
                            :width 100%)
                          '(.footnum
                            :width 3em
                            :float left
                            :font-size 125%
                            :font-weight bold)
                          '(.footpara
                            :margin-right 3em)
                          '(\#postamble
                            :color gray
                            :border-top 0.3125em solid black
                            :padding-top 0.125em
                            :font-size 0.75em
                            :max-width 105em)
                          '(\#table-of-contents
                            :display none)
                          '(\#text-table-of-contents
                            :font-size smaller)
                          '((:or li ul)
                            :margin-bottom 0.5em
                            :margin-top 0.5em
                            :text-align justify
                            :text-justify inter-word)
                          '((:or body .outline-1 .outline-2 .outline-3 .outline-4)
                            :max-width 60ch
                            :line-height 1.4
                            :display block
                            :margin-left auto
                            :margin-right auto)
                          '(video
                            :max-width 60ch
                            :height auto
                            :border 0.125em solid black)
                          '(p
                            :text-align justify
                            :text-justify inter-word
                            :display block
                            :margin-left auto
                            :margin-right auto)
                          '(img
                            :max-width 100%
                            :display block
                            :width auto
                            :height auto
                            :margin-left auto
                            :margin-right auto)
                          '(figure
                            :max-width 100%
                            :display block
                            :margin-left auto
                            :margin-right auto
                            :width auto
                            :height auto)
                          '(a
                            :text-decoration none)
                          '(pre
                            :overflow-x auto)
                          '((:and \.src |::before|)
                            :border 2px solid black
                            :background-color \#aeeeee
                            :display block
                            :font-size 0.75em
                            :padding 0.125em
                            :margin-bottom 0.5em
                            :text-align center)
                          '((:and \.src-c |::before|)
                            :content "'C'")
                          '((:and \.src-cpp |::before|)
                            :content "'C++'")
                          '((:and \.src-elisp |::before|)
                            :content "'Emacs Lisp'")
                          '((:and \.src-lisp |::before|)
                            :content "'Common Lisp'")
                          '((:and \.src-scm |::before|)
                            :content "'Scheme'")
                          '((:and \.src-octaspire-dern |::before|)
                            :content "'Dern'")
                          '((:and \.src-s |::before|)
                            :content "'GNU Assembly'")
                          '((:and \.src-asm |::before|)
                            :content "'Intel Assembly'")
                          '((:and \.src-diff |::before|)
                            :content "'Diff'")
                          '((:and \.src-shell |::before|)
                            :content "'Shell'")
                          '((:and \.src-css |::before|)
                            :content "'CSS'")
                          '((:and \.src-make |::before|)
                            :content "'Make'")
                          '((:and \.src-json |::before|)
                            :content "'JSON'")
                          '((:and \.src-yaml |::before|)
                            :content "'YAML'")
                          '((:and \.src-js |::before|)
                            :content "'JavaScript'")
                          '((:and \.src-java |::before|)
                            :content "'Java'")
                          '((:and \.src-clj |::before|)
                            :content "'Clojure'")
                          '((:and \.src-python |::before|)
                            :content "'Python'")
                          '((:and \.src-xml |::before|)
                            :content "'XML'")
                          '((:and \.src-proto |::before|)
                            :content "'Protocol Buffer'")
                          '(.example
                            :padding-top 0.5em)
                          '(pre
                            :padding-top 0px
                            :padding-bottom 0.5em
                            :margin-top 0px)
                          '((:or code pre)
                            :font-family "'IBM Plex Mono'" monospace
                            :background-color \#efefcc)
                          '(aside
                            :margin-top 1em
                            :margin-bottom 1em
                            :padding 1em
                            :border 0.125em dashed black)
                          '(table
                            :margin-top 1em
                            :margin-bottom 1em
                            :max-width 100%
                            :min-width 100%
                            :line-height 1.4
                            :border-collapse collapse
                            :text-align justify
                            :text-justify inter-word)
                          '(th
                            :background-color \#aeeeee)
                          '((:or tr td th)
                            :border 0.125em solid black)
                          '(.org-src-container
                            :padding-top 0px
                            :margin-top 0px)
                          '(.src-copy-button
                            :font-family "'IBM Plex Mono'" monospace
                            :margin 0px)
                          '(.todo
                            :color darksalmon
                            :font-weight bold
                            :font-family "'IBM Plex Mono'" monospace)
                          '(.done
                            :color darkgreen
                            :font-weight bold
                            :font-family "'IBM Plex Mono'" monospace)
                          '(:media "(max-width: 640px)"
                            ((:or nav \#footnotes \#postamble \#preamble)
                             :max-width 40ch
                             :margin-left 10px)
                            ((:or body .outline-1 .outline-2 .outline-3 .outline-4)
                             :max-width 40ch
                             :margin-left 10px)
                            (video
                             :max-width 40ch
                             :margin-left 10px))))

(lass:define-special-property src (&rest files)
  (list (lass:make-property "src" (format nil "url(data:font/ttf;base64,~A) format('truetype')" (file-as-base64 (car files))))))

(defun generate ()
  (let ((sheet (apply #'lass:compile-sheet *css*)))
    (with-open-file (stream "style.css"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-prefix stream)
      (lass:write-sheet
       sheet
       :stream stream
       :pretty t))
    (with-open-file (stream "base64-style.org"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-org-prefix stream)
      (write-prefix stream :one-line t)
      (lass:write-sheet
       sheet
       :stream stream
       :pretty nil)
      (write-org-postfix stream))))

