(ns mloid.lex)

(defn- ignore [re] [re (constantly nil)])
(defn- lit [name re] [re (constantly [name])])
(defn- tok [name re] [re (fn [x] [[name x]])])

;; A list of [re handler] pairs, where `re' is a regex without capturing groups
;; and `handler' is a function that takes the string matched by `re' and returns
;; a sequence of tokens.
(def ^{:private true} tokenizers
  (list
    (ignore #"\s+")                   ; whitespace
    (ignore #"#.*(?:\n|$)")           ; comment

    (lit :lparen #"\(")  (lit :rparen #"\)")
    (lit :lsquare #"\[") (lit :rsquare #"\]")
    (lit :lcurly #"\{")  (lit :rcurly #"\}")

    (tok :ident     #"[a-zA-Z][a-zA-Z0-9_]*")
    ;; need to put num before symbol so that "-23" parses correctly
    (tok :num       #"[+-]?\d+(?:\.\d+)?")
    (tok :symbol    #"[~!@$%^&*=+\\:<>/?|,.;-]+")

    ;; strings require post-processing to deal with escape sequences we defer
    ;; this (metacircularly) to clojure's string syntax. TODO: match more escape
    ;; sequences
    [#"\"(?:[^\"\\]|\\[\\\"])*\""
      (fn [s] [[:string (read-string s)]])]
    ))

;; Take a reader
(defn- re-looking-at
  ([re s] (re-looking-at re s 0))
  ([re s from-index]
    (let [m (re-matcher re s)]
      (.region m from-index (count s))
      (if (. m (lookingAt))
        (re-groups m)))))

(defn tokenize-string [str from-index]
  (when (< from-index (count str))
    (lazy-seq
      (loop [[[re handler] & rest] tokenizers]
        (if-let [tokstr (re-looking-at re str from-index)]
          (concat (handler tokstr)
            (tokenize-string str (+ from-index (count tokstr))))
          (if (empty? rest)
            ;; TODO: better error
            (throw (Exception. "no matching regex in lexer"))
            (recur rest)))))))

(defn tokenize [rdr]
  (tokenize-string (slurp rdr) 0))
