(jde-project-file-version "1.0")

(defsubst webhead-classpath ()
  "Builds webhead classpath."
  (append
   (list "/usr/share/java/bsh.jar"
         (expand-file-name "java/build/webhead.jar" 
                           (expand-file-name "../" (file-name-directory load-file-name))))
   (directory-files
    (expand-file-name "java/lib" webhead-directory)
    'full
    ".jar")))

(jde-set-variables
 '(jde-global-classpath
   (webhead-classpath)))
