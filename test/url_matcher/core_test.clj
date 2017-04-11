(ns url-matcher.core-test
  (:require [clojure.test :refer :all]
            [url-matcher.core :refer :all]))

(deftest new-pattern-test
  (testing

    (is (= {:host "dribbble.com" :path "shots/?id" :queryparams '("offset=?offset")}
           (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset)")))

    (is (= {:host "dribbble.com" :path "shots/?id" :queryparams '("offset=?offset" "list=?type")}
           (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type)")))

    (is (= {:host "twitter.com" :path "?user/status/?id" :queryparams '()}
           (new-pattern "host(twitter.com); path(?user/status/?id);")))

    ))

(deftest recognize-test
  (testing

    (let [dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset)")]
      (is (= '([:id "1905065-Travel-Icons-pack"] [:offset "1"])
             (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))))

    (let [dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset)")]
      (is (nil? (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"))))

    (let [dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset)")]
      (is (nil? (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))))

    (let [dribbble2 (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type)")]
      (is (= '([:id "1905065-Travel-Icons-pack"] [:offset "1"] [:type "users"])
             (recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))))

    (let [twitter (new-pattern "host(twitter.com); path(?user/status/?id);")]
      (is (= '([:user "bradfitz"] [:id "562360748727611392"])
             (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392"))))

    ))
