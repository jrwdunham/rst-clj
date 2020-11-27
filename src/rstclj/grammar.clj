(ns rstclj.grammar
  "This namespace contains Instaparse CFG (or PEG) grammars (and parsers derived
  therefrom) that are used by RST-CLJ."
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.set :as set]))

(def whitespace-prefix-grmr "WHITESPACE_PREFIX = #'^( |\\t)+'\n")

(def empty-line-grmr "EMPTY_LINE = #'\\s*\\n'\n")

(def any-char-grmr "< ANY_CHAR > = #'.'\n")

(def word-char-grmr "< WORD_CHAR > = #'\\w'\n")

(def content-grmr
  (str "CONTENT = ANY_CHAR* WORD_CHAR ANY_CHAR*\n"
       any-char-grmr
       word-char-grmr))

(def newline-grmr "NEWLINE = '\\n'\n")

(def sample-bullet-list
  (str/join
   \newline
   ["- This is the first bullet list item.  The blank line above the"
    "  first list item is required; blank lines between list items"
    "  (such as below this paragraph) are optional."
    ""
    "- This is the first paragraph in the second item in the list."
    ""
    "  This is the second paragraph in the second item in the list."
    "  The blank line above this paragraph is required.  The left edge"
    "  of this paragraph lines up with the paragraph above, both"
    "  indented relative to the bullet."
    ""
    "  - This is a sublist.  The bullet lines up with the left edge of"
    "    the text blocks above.  A sublist is a new list so requires a"
    "    blank line above and below."
    ""
    "- This is the third item of the main list."
    ""
    "This paragraph is not part of the list."]))

;; "*", "+", "-", "•", "‣", or "⁃", 
(def bullet-item-grmr
  (str "BULLET_ITEM = "
       "WHITESPACE_PREFIX? BULLET ' ' CONTENT < NEWLINE > "
       "( '  ' WHITESPACE_PREFIX? CONTENT < NEWLINE > )*"
       "BULLET = ( '*' | '+' | '-')\n"
       content-grmr
       newline-grmr
       whitespace-prefix-grmr))

(def header-underline-grmr
  (str "HEADER_LINE = #'("
       "={4,1000}|"
       "-{4,1000}|"
       "`{4,1000}|"
       ":{4,1000}|"
       "\\'{4,1000}|"
       "\\\"{4,1000}|"
       "~{4,1000}|"
       "\\^{4,1000}|"
       "_{4,1000}|"
       "\\*{4,1000}|"
       "\\+{4,1000}|"
       "#{4,1000}|"
       "<{4,1000}|"
       ">{4,1000})\\n'\n"))

(def header-text-grmr "HEADER_TEXT = #'.*\\w+.*\\n'\n")

(def header-grmr
  (str "HEADER = "
       "( HEADER_TEXT HEADER_LINE ) |"
       "( HEADER_LINE < #'( |\\t)*' > HEADER_TEXT HEADER_LINE )\n"
       header-text-grmr
       header-underline-grmr))

(def code-block-sigil-grmr "CODE_BLOCK_SIGIL = '::'\n")

(def content-line-pre-code-block-grmr
  (str "CONTENT_LINE_PRE_CODE_BLOCK = CONTENT CODE_BLOCK_SIGIL < NEWLINE >\n"
       content-grmr
       code-block-sigil-grmr
       newline-grmr))

(def content-line-grmr
  (str "CONTENT_LINE = CONTENT < NEWLINE >\n"
       content-grmr
       newline-grmr))

(def paragraph-grmr
  (str "PARAGRAPH = < EMPTY_LINE+ > CONTENT_LINE* CONTENT_LINE\n"
       empty-line-grmr
       content-line-grmr))

(def paragraph-pre-code-block-grmr
  (str "PARAGRAPH_PRE_CODE_BLOCK = < EMPTY_LINE > "
                                  "CONTENT_LINE* "
                                  "CONTENT_LINE_PRE_CODE_BLOCK\n"
       empty-line-grmr
       content-line-pre-code-block-grmr
       content-line-grmr))

(def code-block-line-grmr
  (str "CODE_BLOCK_LINE = WHITESPACE_PREFIX CONTENT < NEWLINE >\n"
       whitespace-prefix-grmr
       content-grmr
       newline-grmr))

(def code-block-grmr
  (str "CODE_BLOCK = < EMPTY_LINE+ > CODE_BLOCK_LINE+\n"
       code-block-line-grmr))

(def rst-grmr
  (str "RST = ( HEADER | "
               "PARAGRAPH | "
               "( PARAGRAPH_PRE_CODE_BLOCK CODE_BLOCK ) | "
               "< EMPTY_LINE > )*\n"
       header-grmr
       paragraph-grmr
       code-block-grmr
       paragraph-pre-code-block-grmr))

(defn normalize-grammar [grmr]
  (->> grmr
       str/split-lines
       distinct
       (str/join \newline)))

(def rst-prsr (insta/parser rst-grmr))

(defmulti process-node (fn [[tag & _]] tag))

(defmethod process-node :RST [[_ & children]]
  (mapv process-node children))

(defmethod process-node :HEADER [[_ [_ a] [_ b] [_ c]]]
  (if c
    {:type :header
     :text (str/trim b)
     :level (keyword (str "level-double-" (first a)))}
    {:type :header
     :text (str/trim a)
     :level (keyword (str "level" (first b)))}))

(defn process-paragraph-children [children]
  {:type :paragraph
   :children (mapv process-node children)})

(defmethod process-node :PARAGRAPH_PRE_CODE_BLOCK [[_ & children]]
  (process-paragraph-children children))

(defmethod process-node :PARAGRAPH [[_ & children]]
  (process-paragraph-children children))

(defn process-content [content]
  (str/join "" content))

(defmethod process-node :CONTENT_LINE [[_ [_ & content] & _]]
  (process-content content))

(defmethod process-node :CONTENT_LINE_PRE_CODE_BLOCK [[_ [_ & content] & _]]
  (process-content content))

(defn- normalize-lines [lines]
  (let [smallest-indent (->> lines
                             (map (comp count first))
                             (reduce min 12))]
    (mapv (fn [[indent line]]
            (str (apply str (drop smallest-indent indent)) line))
          lines)))

(defmethod process-node :CODE_BLOCK [[_ & lines]]
  {:type :code
   :lines (->> lines
               (mapv (fn [[_ [_ white-space] [_ & content]]]
                       [white-space (apply str content)]))
               normalize-lines)})

(defmethod process-node :default [node] node)

(defn set-header-levels [doc]
  (let [key (->> doc
                 (filter #(= :header (:type %)))
                 (map :level)
                 distinct
                 (interleave (range))
                 (apply hash-map)
                 set/map-invert)]
    (map (fn [{:keys [type] :as e}]
           (if (= :header type) (update e :level key) e))
         doc)))

(comment

  (->> "sample.rst"
       clojure.java.io/resource
       slurp
       (insta/parse rst-prsr)
       process-node
       set-header-levels)

)
