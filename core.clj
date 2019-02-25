(ns depcalculator.core
  (:require [clojure.java.io :as io])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.algo.monads :as m])
  ;(:import (java.lang.Character))
  ;(:import (java.lang.Double))
  ;(:import (java.lang.System))
  (:gen-class)
  )


;;decimal numbers don't parse for some reason.

(declare lexer)
(defn variable
  ([{:keys [res prevV] :as ret} c]

   (if (re-matches #"\w" c)
     (do
       (def n-prevV (str prevV c))
       (cond
         (= n-prevV "let") (-> ret
                               (assoc-in [:prevV] [])
                               (assoc-in [:res] (conj res {:let n-prevV})))
         
         (= n-prevV "in") (-> ret
                              (assoc-in [:prevV] [])
                              (assoc-in [:res] (conj res {:in n-prevV})))
         
         :else (assoc-in ret [:prevV] n-prevV)))
     (lexer (variable ret) c)
     ))
  ([{:keys [res prevV] :as ret}]
   (-> ret
       (assoc-in [:res] (conj res {:variable prevV}))
       (assoc-in [:prevV] [])
       )
   )
  )

(defn digit
  ([{:keys [res prevD] :as ret} c]
   (cond
     (re-matches #"\d|\." c) (assoc-in ret [:prevD] (str prevD c))
     ;;(= c ".") (assoc-in ret [:prevD] (str prevD c))
     :else (lexer (digit ret) c)
     ))
  ([{:keys [res prevD] :as ret}]
   ;;(print prevD)
   (-> ret
       (assoc-in [:res] (conj res {:number (Double/parseDouble prevD)}))
       (assoc-in [:prevD] [])
       )
   )
  )


#_(
   "
   lexical syntax:
   
   <lexeme> ::= <numeric constant> | <variable name> | let | in | = | + | - | * | / | ( | )
   <variable name> ::= <first character> <rest of the variable name>
   <first character> ::= <letter> | _
   <rest of the variable name> ::= <empty> | <name character> <rest of the variable name>
   <name character> ::= <letter> | <digit> | _
   <numeric constant> ::= <integer constant> | <float constant>
   <integer constant> ::= <digits>
   <digits> ::= <digit> | <digit> <digits>
   <float constant> ::= <digits> . | . <digits> | <digits> . <digits>

   types of lexemes:
   {"=" :equals}
   {"+" :plus}
   {"-" :minus}
   {"*" :asterisk}
   {"/" :solidus}
   {"(" :oparen}
   {")" :cparen}
   {"let" :let}
   {"in" :in}
   {"10.00" :number}
   {"_asdasd5" :variable}
   {"END" :end}
   "
   )

(defn lexer
  "takes a single characters and combines them to lexems
  3 parameter version handles longer lexems"
  ([{:keys [res prevV prevD error] :as ret} c]
   ;;(println ret)
   (cond
     (not (empty? error)) ret
     (not (empty? prevV)) (variable ret c )
     (not (empty? prevD)) (digit ret c)
     :else (condp = c
             "=" (assoc-in ret [:res] (conj res {:equals "="}))
             "+" (assoc-in ret [:res] (conj res {:plus "+"}))
             "-" (assoc-in ret [:res] (conj res {:minus "-"}))
             "*" (assoc-in ret [:res] (conj res {:asterisk "*"}))
             "/" (assoc-in ret [:res] (conj res {:solidus "/"}))
             "(" (assoc-in ret [:res] (conj res {:oparen "("}))
             ")" (assoc-in ret [:res] (conj res {:cparen ")"}))
             (or (some #(if (some? %) %)
                       [(if (re-matches #"\d" c)
                          (assoc-in ret [:prevD] c))
                        (if (re-matches #"[a-zA-Z_]" c)
                          (assoc-in ret [:prevV] c))])
                 (assoc-in ret [:error] c))
             )
     )
   )

  ([{:keys [res prevV prevD error] :as ret}]
   ;;(print ret)
   (cond
     (not (empty? error)) ret
     (not (empty? prevV)) (recur (variable ret))
     (not (empty? prevD)) (recur (digit ret))
     :else (assoc-in ret [:res] (conj res {:end "END"})))
   )
  )


#_(
   "
   Abstract syntax (AST):
   ​E,F ​​∈​ ​​​Exp
   ​E,F ​::=​ ​E+F
   ∣​​E−F
   ∣​E∗F​
   ∣E/F
   ∣​−E
   ​∣​c
   ​∣​x
   ​∣​let x=E in F​
   ​c,d ​∈ Const​
   x,y,z​ ​∈ Var​​
"
   )


(m/defmonad error-m
  [m-result (fn [v] v)
   m-bind (fn [v f]
            (println v)
            (if (#(contains? % :error) v)
              v
              (f v)))
   ]
  )

(m/defmonad error-m-m ;;m as in modified for real use and m as in monad. Not a good name but I'm out of creativity.
  [m-result (fn [v] v)
   m-bind (fn [v f]
            (if  (#(if (seq? %) (contains? (first %) :error) false) v)
              v
              (f v)))]
  )

(def state-in-error-m (m/state-t error-m-m))

(defn getOne [a & as]
  [a as])

(defn separ [sepf]
  (fn [s]
    (sepf s)))

(defn error? [l]
  (= :error (first l)))

(defn spawn-error [k l]
  (cons :error l))

(defn pass [f t]
  [(f t)])

(defn look [l]
  "gets you the next key from list l"
  (first (keys (first l))))

;;This could have been done differently. Wanted to try this language feature. Does not fit well here it seems now.
;;Makes working a bit more haskel like but not really.
(defmulti handlek (fn [k s] k))
(defmethod handlek :variable
  [k s]
  (apply getOne s))
(defmethod handlek :equals
  [k s]
  (apply getOne s))
(defmethod handlek :in
  [k s]
  (apply getOne s))
(defmethod handlek :end
  [k s]
  (apply getOne s))
(defmethod handlek :cparen
  [k s]
  (apply getOne s))

(defn expect [k]
  ;;handle error
  ;;() ;cond error? so on
  #(if (= k (look %)) ;; % is the state (or lexem list) and of form [{:key value} ... ]
     (handlek k %)
     ([(spawn-error k %) %])
     )
  )

#_(
   "
   types of lexemes:
   {"=" :equals}
   {"+" :plus}
   {"-" :minus}
   {"*" :asterisk}
   {"/" :solidus}
   {"(" :oparen}
   {")" :cparen}
   {"let" :let}
   {"in" :in}
   {"10.00" :number}
   {"_asdasd5" :variable}
   {"END" :end}


<expression> ::= <term>
               | let <variable name> = <expression> in <expression>

<term> ::= <factor>
         | <term> + <factor>
         | <term> - <factor>
         
<factor> ::= <unary expression>
           | <factor> * <unary expression>
           | <factor> / <unary expression>

<unary expression> ::= <primary expression>
                     | + <unary expression>
                     | - <unary expression>

<primary expression> ::= <numeric constant>
                       | <variable name>
                       | ( <expression> )
   "
   )

(declare expr)
(defn primary []
  #(case (look %)
     
     :number [(:number (first %)) (drop 1 %)]
     
     :variable (let [val (:variable (first %))]
                 [`(:unboundvar ~val) (drop 1 %)])
     
     :oparen ((m/domonad state-in-error-m
                         [rv (expr)
                          cparen (expect :cparen)]
                         rv)(drop 1 %))
     
     [{:error (look %)} %]
     )
  )


(defn unary []
  #(case (look %)
     
     :plus ((m/domonad state-in-error-m
                       [out (unary)]
                       out) (drop 1 %))
     
     :minus ((m/domonad state-in-error-m
                        [out (unary)]
                        `(- ~out)) (drop 1 %))
     
     ((m/domonad state-in-error-m
                 [out (primary)]
                 out) %)))


(defn factor-separator? [left]
  #(case (look %)
     
     :asterisk ((m/domonad state-in-error-m
                           [right (unary)
                            out (factor-separator? `(* ~left ~right))]
                           out) (drop 1 %))
     
     :solidus ((m/domonad state-in-error-m
                          [right (unary)
                           out (factor-separator? `(/ ~left ~right))]
                          out) (drop 1 %))
     [left %]))

(defn factor []
  #((m/domonad state-in-error-m
               [left (unary)
                out (factor-separator? left)]
               out) %))


(defn term-separator? [left]
  #(case (look %)
     
     :plus ((m/domonad state-in-error-m
                       [right (factor)
                        out (term-separator? `(+ ~left ~right))]
                       out) (drop 1 %))
     
     :minus ((m/domonad state-in-error-m
                        [right (factor)
                         out (term-separator? `(- ~left ~right))]
                        out) (drop 1 %))
     
     [left %]))

(defn term []
  (m/domonad state-in-error-m
               [left (factor)
                out (term-separator? left)]
               out))

(defn loopper [var val exr]
  (if (not (coll? exr))
    exr
    (case (first exr)
      
      :unboundvar (if (= var (nth exr 1))
                    val
                    exr)
      
      `run-expr (loopper var val (macroexpand exr))

      
      (map #(if (coll? %)
              (loopper var val %)
              %) exr)
      ))
  )

;;Makes A couple of additional list go troughs but I was running out of time.
(defmacro run-expr [var expr1 expr2]
  (let [val (eval expr1)] ;;breaks stuff because eval?
    (loopper (:variable var) val expr2)) ;;should return unevaluated form!
  )

;;All parser functions are functions that return a function. % is the paremeter of the return function (or the state of the state monad for which it is needed.)
(defn expr []
  #(case (look %)

     ;lexem list is handled by the state monad.
     :let ((m/domonad state-in-error-m 
                      [var (expect :variable)
                       eq (expect :equals)
                       expr1 (expr) 
                       in (expect :in)
                       expr2 (expr)]
                      `(run-expr ~var ~expr1 ~expr2)) (drop 1 %))
     
     ((m/domonad state-in-error-m
                 [out (term)]
                 out) %))
  )

(defn parse [input]
  (eval (first  ((m/domonad state-in-error-m
                     [exr (expr)
                      end (expect :end)]
                     exr) input))))


(defn runFromString
  "Does stuff."
  [s]

  ;;Lexer:
  (->> s
       (seq)
       (map str)
       (r/filter #(nil? (re-matches #"\s" %)))
       ;;r/reduce = strict left fold
       (r/reduce lexer {:prevD [] :prevV [] :res [] :error []})
       (lexer)
       (def lexerResult)
       )

  ;;error check
  (def resultForAST
    (cond
      (not (empty? (lexerResult :error))) (do
                                            (println "Error in parsing lexems")
                                            (println lexerResult))
      :else (lexerResult :res)
      ))

  ;;(println resultForAST)

  (parse resultForAST)

  )

(def testStrings [;;"-1" ;-1
                  ;;"1+1" ;2
                  ;;"1+2*2+3" ;8
                  ;;"2*1+1*3" ;5
                  ;;"1/2+3/4" ;5/4
                  ;;"3*2/3*(-4)" ;-8
                  ;;"1*(2+1)*2" ;6
                  ;;"(2*2)*-(1+3)" ;-16
                  ;;"let x = 1 in x" ;1
                  ;;"let x = 6 in (1+x-3)" ;4
                  ;;"let x = 1+1 in (x+2)" ;4
                  ;;"let x = 3*2 in 2*2+x+3" ;13
                  ;;"let var = (1+1) in (var+2)" ;4
                  ;;"1+(let x = 3 in x + 1)" ;5
                  ;;"let x = 1 in let y = 2 in x + y" ;3
                  ;;"let x = 1 in x + (let y = 2 in y)" ;3
                  ;;"let x = (let k=1 in k+1) in x + 1" ;3
                  ;;"let var = (let x = 1 in 1+x) in var * 1 + 2 * 2 - (let y = 1 in (let z = 2 in y + z ))" ;3
                  ;;"let x = let x = 3 in x - 2 in let x = 2 in x + x"
                  ])

(defn runtest []
  (map runFromString testStrings))

;;(runtest)


#_(defn usingInputStream
  "Not opretational. I'm not sure how to make command line reading work with clojure this way.
Im out of time"
  [in]
  
  (with-open [inp (io/input-stream in)]
     (loop [instring ""]
      (let [ch (str(. inp read))]
        (if ((= ch -1))
          instring
          (recur (conj instring ch))
          )))
    )
  )

(defn -main
  "Starts from here I guess"
  [& args]
  (println "I'm a calculator. beep! boop!")
  (println "Quit with quit, exit or :q")
  (loop [a "a"]
      (let [inp (read-line)]
        (if (some #(= inp %)["exit" "quit" ":q"])
          nil
          (do
            (println (runFromString inp))
              (recur "a"))))
    )
  )


