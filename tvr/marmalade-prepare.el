;; -*- lexical-binding: nil; -*-
(when (boundp 'package-archives)
  (add-to-list
   'package-archives
   '("marmalade"
     . "http://marmalade-repo.org/packages/"))
  )
