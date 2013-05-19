(ns leiningen.pedestal.uberwar
  (:require [leiningen.core.classpath :refer (get-classpath)]
            [leiningen.pedestal.war :as war]
            [clojure.java.io :as io]))

(defn jar-dependencies [project]
  (for [pathname (get-classpath project)
        :let [file (io/file pathname)
              fname (.getName file)]
        :when (.endsWith fname ".jar")]
    file))

(defn jar-entries [project war]
  (doseq [jar-file (jar-dependencies project)]
    (let [dir-path (.getParent jar-file)
          war-path (war/in-war-path "WEB-INF/lib/" dir-path jar-file)]
      (war/file-entry war project war-path jar-file))))

(defn uberwar
  "Create a $PROJECT-$VERSION.war file with all the dependencies."
  ([project]
   (uberwar project (get-in project [:predestal :uberwar-name]
                            (war/war-name project "-standalone"))))
  ([project uberwar-name-str]
   (let [war-path (war/war-file-path project uberwar-name-str)]
     (war/write-war project war-path jar-entries)
     (println "Created" war-path)
     war-path)))

