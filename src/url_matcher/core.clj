;;
;; Matcher should recognize and destruct URL by:
;; host: domain
;; path: parts, splitted with "/"
;; queryparam: name/value pairs of query
;; (see examples below)
;;
;; Each string that is started from "?" is a "bind"
;; (recognize matcher) should return nil or seq of binds
;;
;; sample1:
;; "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type)"      pattern
;; "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"                     url
;; (more samples in core_test)
;;

(ns url-matcher.core
  (:require [clojure.string :as str]))

(defn new-pattern [pattern]
  (let [parts (str/split pattern #";")
        [_, host] (re-find #"host\(([^\)]*)" pattern) ;; sample1 -> dribbble.com
        [_ path] (re-find #"path\(([^\)]*)" pattern) ;; sample1 -> shots/?id
        queryparams (map (fn [[_, qp]] qp) (re-seq #"queryparam\(([^\)]*)" pattern))] ;; sample1 -> ("offset=?offset" "list=?type")
    (as-> {} res
      (if-not (nil? host) (conj res [:host host]))
      (if-not (nil? path) (conj res [:path path]))
      (conj res [:queryparams queryparams])
    )))

(defn- recognize_host_and_path [host path sample]
  (let [path_regexp (str/replace path #"\?[^\/]+" "([^/\\\\?]+)")]  ;; shots/?id -> shots/([^/\\?]+)
    (if-let [[_ & path_binding_values]
             (re-find (re-pattern (str ".+://" host "/?" path_regexp)) sample)] ;; match host and bind path_args
      (let [path_binding_names (map keyword (re-seq #"(?<=\?)[^\/]+" path))] ;; shots/?id -> (:id)
        (map vector path_binding_names path_binding_values)))))

(defn- recognize_queryparams [pattern sample]
  (let [[_ queryparam_sample] (re-find #"\?(.*)" sample)]
    (loop [[queryparam & tail] pattern
           acc []]
      (if (nil? queryparam)
        acc
        (let [[_ parameter var_name] (re-find #"(.+)=\?(.+)" queryparam) ;; list=?type -> body: "list=", name: "type"
              binding_name (keyword var_name)]
          (if-let [[_ binding_value] (re-find (re-pattern (str parameter "=([^&]+)")) queryparam_sample)] ;; list=users&offset=1 -> users
            (recur tail (conj acc [binding_name binding_value]))
            nil))))))

(defn recognize [pattern sample]
  (let [{:keys [host path queryparams] :or {host ".+" path ""}} pattern]
    (if-let [path_bindings (recognize_host_and_path host path sample)]
      (if-let [queryparams (recognize_queryparams queryparams sample)]
        (concat path_bindings queryparams)))))
