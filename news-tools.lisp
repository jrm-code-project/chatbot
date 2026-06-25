;;; -*- Lisp -*-
;;; news-tools.lisp - convenience helpers for news and feed-driven prompts

(in-package "CHATBOT")

(defparameter *lisp-news-feed-urls*
  '("https://planet.lisp.org/rss20.xml"
    "https://planet.scheme.org/atom.xml")
  "Feed URLs consulted by LISP-NEWS.")

(defun lisp-news (&key conversation callback)
  "Asks the active conversation for recent Lisp and Scheme news using the configured feeds."
  (chat "What's new in the world of Lisp and Scheme these days?"
        :conversation conversation
        :callback callback
        :files *lisp-news-feed-urls*))
