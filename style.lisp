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

(if (string= "WIDE" (uiop:getenv "OCTASPIRE_CSS_WIDTH"))
    (defparameter *maxw* '120ch)
    (defparameter *maxw* '60ch))

(defparameter *maxw-small* '50ch)


(defun file-as-base64 (path)
  (cl-base64:usb8-array-to-base64-string (alexandria:read-file-into-byte-vector path)))

(defparameter *css* (list '(@font-face
                            :font-family "'IBM Plex Sans'"
                            :src "external/IBM/Plex/TrueType/Sans/IBMPlexSans-Regular.ttf")
                          '(@font-face
                            :font-family "'IBM Plex Mono'"
                            :src "external/IBM/Plex/TrueType/Mono/IBMPlexMono-Regular.ttf")
                          '(body
                            :background-color "#ffffea"
                            :color "black"
                            :font-family "'IBM Plex Sans'" "sans-serif")
                          '(.toc-toggle-button
                            :font-family "'IBM Plex Mono'" monospace
                            :margin 0)
                          `((:or nav \#footnotes \#postamble \#preamble)
                            :display block
                            :line-height 1.4
                            :margin-left auto
                            :margin-right auto
                            :max-width ,*maxw*)
                          '(nav
                            :border-bottom 0.25em solid black
                            :border-top 0.25em solid black)
                          '(\#footnotes
                            :border-top 0.25em solid black
                            :margin-top 2em)
                          '(.footdef
                            :width 100%)
                          '(.footnum
                            :float left
                            :font-size 125%
                            :font-weight bold
                            :width 3em)
                          '(.footpara
                            :margin-right 3em)
                          '(\#postamble
                            :border-top 0.3125em solid black
                            :color gray
                            :font-size 0.75em
                            :max-width 105em
                            :padding-top 0.125em)
                          '(\#table-of-contents
                            :display none)
                          '(\#text-table-of-contents
                            :font-size smaller)
                          '((:or li ul)
                            :margin-bottom 0.5em
                            :margin-top 0.5em
                            :text-align justify
                            :text-justify inter-word)
                          `((:or body .outline-1 .outline-2 .outline-3 .outline-4)
                            :display block
                            :line-height 1.4
                            :margin-left auto
                            :margin-right auto
                            :max-width ,*maxw*)
                          `(video
                            :border 0.125em solid black
                            :height auto
                            :max-width ,*maxw*)
                          '(blockquote
                            :background-color \#aeeeee
                            :border-left solid 0.25em black
                            :padding 0.25em)
                          '("blockquote p"
                            :font-size 1.0em
                            :padding 0.125em 1em 0.125em 1em)
                          '((:and blockquote |::before|)
                            :float left
                            :color black
                            :content open-quote
                            :font-size 2em
                            :vertical-align -0.3em)
                          '(\.title
                            :margin 0
                            :padding 0)
                          '(\.subtitle
                            :font-size 1.25em
                            :font-weight bold
                            :margin 0
                            :padding 0)
                          '(p
                            :display block
                            :margin-left auto
                            :margin-right auto
                            :text-align justify
                            :text-justify inter-word)
                          '(img
                            :display block
                            :height auto
                            :margin-left auto
                            :margin-right auto
                            :max-width 100%
                            :width auto)
                          '(figure
                            :display block
                            :height auto
                            :margin-left auto
                            :margin-right auto
                            :max-width 100%
                            :width auto)
                          '(a
                            :text-decoration none)
                          '(pre
                            :overflow-x auto)
                          '((:and \.src |::before|)
                            :background-color \#aeeeee
                            :border 2px solid black
                            :display block
                            :font-size 0.75em
                            :margin-bottom 0.5em
                            :padding 0.125em
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
                            :margin-top 0
                            :padding-bottom 0.5em
                            :padding-top 0)
                          '((:or code pre)
                            :background-color \#efefcc
                            :font-family "'IBM Plex Mono'" monospace)
                          '(aside
                            :border 0.125em dashed black
                            :margin-bottom 1em
                            :margin-top 1em
                            :padding 1em)
                          '(table
                            :border-collapse collapse
                            :line-height 1.4
                            :margin-bottom 1em
                            :margin-top 1em
                            :max-width 100%
                            :min-width 100%
                            :text-align justify
                            :text-justify inter-word)
                          '(th
                            :background-color \#aeeeee)
                          '((:or tr td th)
                            :border 0.125em solid black)
                          '(.org-src-container
                            :margin-top 0
                            :padding-top 0)
                          '(.src-copy-button
                            :font-family "'IBM Plex Mono'" monospace
                            :margin 0)
                          '(.todo
                            :color darksalmon
                            :font-family "'IBM Plex Mono'" monospace
                            :font-weight bold)
                          '(.done
                            :color darkgreen
                            :font-family "'IBM Plex Mono'" monospace
                            :font-weight bold)
                          `(:media "(max-width: 640px)"
                                   ((:or nav \#footnotes \#postamble \#preamble)
                                    :max-width ,*maxw-small*
                                    :padding-left 10px)
                                   ((:or body .outline-1 .outline-2 .outline-3 .outline-4)
                                    :max-width ,*maxw-small*
                                    :padding-left 10px)
                                   (video
                                    :max-width ,*maxw-small*
                                    :padding-left 10px))))

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

