;; -*- lexical-binding: t; -*-
(when (boundp 'package-archives)
  (add-to-list
   'package-archives
   '("marmalade"
     . "http://marmalade-repo.org/packages/"))
  )
