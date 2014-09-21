(ns 
  ^{:doc "Use AJFX to infer the frame conditions of Java/AspectJ code."
    :author "Tim Molderez" }
  ekeko-ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use [inspector-jay.core]
        [clojure.repl]
        [damp.ekeko logic]
        [damp.ekeko]
        [ekeko-ajfx.soot]
        [ekeko-ajfx.analysis])
  (:import [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph LoopNestTree]))

(defn do-analysis [method]
  (ekeko-ajfx.diagram/reset-obj-id)
  (-> ekeko-ajfx.analysis/started-analysis .clear)
  (infer-frame method))

(defn get-all-advice []
  (set (ekeko [?adv]
         (w/advice ?adv))))

(defn get-all-advice-bodies []
  (set (ekeko [?method]
         (l/fresh [?advice]
           (w/advice ?advice)
           (ajsoot/advice-soot|method ?advice ?method)))))

(defn get-all-bodies []
  (ekeko [?method]
         (l/fresh []
           (jsoot/soot :method ?method))))

(comment
  (inspect (get-all-advice))
  (inspect (get-all-advice-bodies))
  (count (get-all-advice-bodies))
  (inspect (get-all-bodies))
  
  (do-analysis (nth 
                 (for [x (get-all-advice-bodies)] (first x)) 10))
  (inspect (nth (for [x (get-all-advice-bodies)] (first x)) 10))
  
  (inspect (filter
             (fn [x]
               (-> (first x) .getName (.startsWith "ajc$")))
             (get-all-bodies)))
  
  (inspect (for [x (get-all-advice)]
            (-> (first x) .getSourceLocation)))
  )

;(defn advice []
;  (ek/ekeko [?advice]
;            (wea/advice 
;              ?advice)))
;
;(defn shadow []
;  (ek/ekeko [?shadow]
;            (wea/shadow 
;              ?shadow)))
;
;(defn advice-shadow []
;  (ek/ekeko [?advice ?shadow]
;            (wea/advice-shadow 
;              ?advice 
;              ?shadow)))
;
;(defn method []
;  (ek/ekeko [?method]
;            (wea/method 
;              ?method)))
;
;(defn shadow|invocation-method|called
;  "Returns the BcelMethod referenced within the provided shadow"
;  ([] (map 
;        (fn[x](vector 
;                (first x) 
;                (shadow|invocation-method|called (first x)))) 
;        (shadow)))
;  ([shadow] (first(first(filter 
;                          (fn [x] (.contains (.toString shadow) (.toString (first x)))) 
;                          (method))))))
;
;(defn bceladvice|aspect [advice]
;  (ek/ekeko [?aspect]
;            (wea/aspect-advice ?aspect advice)))







(comment
(defn varType-recursive
  [var unitChain]
 "Helper method of varType"
 (let [unit (first unitChain)
       varName (-> var .getName)
       unitRest (rest unitChain)]
   (if (instance? IdentityStmt unit)
     (if (= varName (-> unit .getLeftOp .getName))
       (cond (instance? ThisRef (-> unit .getRightOp)) :this
          (instance? ParameterRef (-> unit .getRightOp)) :parameter)
       (if (not-empty unitRest)
         (varType-recursive var unitRest)
         :local))
     :local)))

(defn varType
  [var method]
  "Determine what kind of variable var is, given that it appears in the body of method."
  (let [units (-> method .getActiveBody .getUnits)]
    (if (instance? JimpleLocal var)
      (varType-recursive var units)
      :global)))

(defn jimpleLocal-parameterIndex
  "Relate the use of a parameter within a method to its index
@param ?local    a JimpleLocal representing a parameter
@param ?method   the JimpleLocal is used within this method
@param ?index    the index of the parameter within the method's interface"
  [?local ?method ?index]
  (l/fresh [?unit]
           (jsoot/soot|method-soot|unit ?method ?unit)
           (jsoot/soot-unit :JIdentityStmt ?unit)
           (equals ?local (.getLeftOp ?unit))     
           (equals ParameterRef (.getClass (.getRightOp ?unit)))
           (equals ?index (.getIndex(.getRightOp ?unit)))))

(defn jimpleLocal-parameterIndex-fun
  "Functional wrapper for jimpleLocal-parameterIndex"
  [?local ?method]
  (first (first (damp.ekeko/ekeko
    [?index]
    (jimpleLocal-parameterIndex ?local ?method ?index)))))

(defn virtualInvoke-parameter
  "Retrieve the nth parameter from an invoke statement / method call
@param invoke  a JInvokeStmt
@param n       parameter index
@return        the requested parameter, usually a JimpleLocal"
  [invoke n]
  (nth (.getUseBoxes invoke) (inc n)))

(defn pullUpFrame
  "Suppose we wish to infer the frame condition of a method a(), and its body contains
a call to method b(). We've inferred the frame condition of B, but we still need to rephrase it
so it can be added to A's frame condition, which is what this function does. In other words, 
this function 'pulls up' the frame
condition of b() to a().
@param call  the call to b() from a() 
@param body  the method body of b()
@param frame the frame axiom of b()
@return  the frame of b() pulled up to the context of a()"
  [call body frame]
  (for [x frame]
    (let [container (first x)]
      (case (varType container body)
        :parameter (let ; Map the formal parameter back to an actual parameter 
                 [index (jimpleLocal-parameterIndex-fun container body )
                  actualParam (virtualInvoke-parameter call index)]
                 (cons actualParam (rest x)))
        :this (cons 
                (first (-> call .getUseBoxes)) ; Replace this with the method call's receiver 
                (rest x)) 
        :local :error ; Frames shouldn't contain locals
        :global x ; No need to change anything
        
               ))))

(defn internalLocal-sourceValue
  "Relate a local that was generated by Jimple to the value that it represents in the Java source code"
  [])

(def inferMethodFrame
  (memoize (fn [method stack] ; We can memoize here, because the frame condition of a method should always be the same..
  "Infer the frame condition of a method.
@param method the SootMethod we want to analyse
@param stack contains the methods that have been traversed so far;
             this is used to avoid infinite recursion when inferring the frame of a recursive method
@return the frame condition is a list of variables that might change, such that this list can be understood by the advice itself
For example, the function could return a list like this:
[
  [aCar, SootField<wheel>],                             ; refers to aCar.wheel , where aCar is a parameter of the advice
  [aTruck, SootField<cargo>, SootField<contents>],      ; aTruck.cargo.contents
  [this, SootField<cargo>]                              ; this.cargo
]"
    (let [directWrites (ekeko [?container ?field]
                    (methodFieldSet-container-field method ?container ?field))
     directCalls (ekeko [?callee ?call ?container]
                    (method-methodCalls method ?callee ?call ?container))
     callFrames (for [x directCalls]
                  (if (some #{(-> x .getUseBoxes .getValue .getMethod)} stack) ; Did we already see this method?
                    [] ; Skip recursive calls
                    (inferMethodFrame 
                      (nth x 0)
                      (concat  stack))))
     pulledUpWrites (map pullUpFrame
                      (for [x directCalls] (nth x 1))
                      (for [x directCalls] (nth x 0))
                      callFrames)]
    (concat 
      directWrites 
      (apply concat pulledUpWrites))))))
  
(defn inferAdviceFrame0
  [advice]
  "Infer the frame condition of an advice. (context,flow&path-insensitive!)
@param advice the advice we want to analyse (can be obtained via w/advice)
@return the frame condition is a list of variables that might change
@see inferMethodFrame"
  (let 
    [directWrites (ekeko [?container ?field]
                    (adviceFieldSet-container-field advice ?container ?field))
     directCalls (ekeko [?callee ?call ?receiver]
                    (advice|methodCall-soot|method advice ?callee ?call ?receiver))
     callFrames (for [x directCalls]
                      (inferMethodFrame (nth x 0) []))
     pulledUpWrites (map pullUpFrame
                         (for [x directCalls] (nth x 1))
                         (for [x directCalls] (nth x 0))
                         callFrames)]
    ; Now simply merge all the frames we found
    (concat 
      directWrites 
      (apply concat pulledUpWrites))))

(defn inferAdviceFrame
  [advice]
  "Infers the frame condition of an advice."
  (let
    [body (first (first 
                   (ekeko [?method]
                          (w/advice advice)
                          (ajsoot/advice-soot|method advice ?method))))
     cflow]
    body
    )) 

;(inspect (ekeko [?advice] (w/advice ?advice)))
;
;(inspect (getAdviceN 1))

(inspect (let [allAdvice (ekeko [?advice] (w/advice ?advice))]
     (inferAdviceFrame0 (first(nth allAdvice 0))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scratch pad ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
;(inspect 
;  (ekeko 
;    [?a ?b ?c]
;    (virtualInvoke-parameter ?a ?b ?c)
;    ))
;
;(inspect 
;  (ekeko 
;    [?a ?b ?c ?d ?e]
;    (method-methodCalls ?a ?b ?c ?d)
;    (equals ?e (.getUseBoxes ?c))))

;(inspect
;  (ekeko [?b]
;   (l/fresh [?a]
;                    (jsoot/soot|method-soot|unit ?a ?b)
;                    (soot|method-name ?a "unused"))))
;
;(let [allAdvice (ekeko [?advice] (w/advice ?advice))]
;     (inferAdviceFrame (first(first allAdvice))))
;
; Get the units of a particular method
;(inspect
;  (damp.ekeko/ekeko
;    [?unit]
;    (jsoot/soot-unit :JIdentityStmt ?unit)))

; Find the calls in a particular method
(inspect 
  (ekeko [?b]
    (l/fresh [?a ?callee ?recv]
             (jsoot/soot|method-soot|unit ?a ?b)
             (soot|method-name ?a "helperMethod")
             (method-methodCalls ?a ?callee ?b ?recv))))

;;;;;;;;;;;;;;;
(ekeko-ajfx.diagram/reset-obj-id)
(-> ekeko-ajfx.analysis/started-analysis .clear)
(let [q (ekeko [?a] (soot|method-name ?a "helper3"))
      method (first (first q))
      frame (infer-frame method)]
  frame)
;;;;;;;;;;;;;;


(inspect (clojure.stacktrace/root-cause *e))

(inspect
  (let [q (ekeko [?a] (soot|method-name ?a "helper3"))
        method (first (first q))]
    (-> method .getActiveBody .getUnits)))


(inspect
  (let [q (ekeko [?a] (soot|method-name ?a "helper3"))
        method (first (first q))]
    method))

(ekeko [?a ?b] (soot|method-name ?a ?b))

(inspect
  (let [q (ekeko [?a] (soot|method-name ?a "helper"))
        method (first (first q))]
    (new LoopNestTree (-> method .getActiveBody))))

(let [q (ekeko [?a] (soot|method-name ?a "helper"))
        method (first (first q))
        body (-> method .getActiveBody)]
    (showBlockCFG body))

(inspect 
  (ekeko
    [?b ?a]
    (l/fresh []
             (jsoot/soot|method-soot|unit ?a ?b)
             (soot|method-name ?a "helper")
             )))

(inspect 
  (ekeko
    [?b ?c]
    (l/fresh [?a]
             (jsoot/soot|method-soot|unit ?a ?b)
             (jsoot/soot-unit :JAssignStmt ?b)
             (soot|method-name ?a "start")
             (equals ?c (.getLeftOp ?b))
             )))

(inspect 
     (ekeko [?a ?b ?c ?d]
                       (l/fresh []
                       (virtMethodCall-receiver ?a ?b)
                       (jsoot/soot|method-soot|unit ?c ?b)
                       (jsoot/soot-method-units ?c ?d)
                       ;(equals ?d (.getSignature ?c)
                               )))

(inspect 
     (ekeko [?b ?c ?d ?e]
                       (l/fresh [?a]
                       (advice-soot|unit ?a ?b)
                       (equals ?c (.getUseBoxes ?b))
                       (equals ?d (type ?b))
                       (jsoot/soot-unit :JIdentityStmt ?b)
                       (equals ?e (.getLeftOp ?b))
                               )))
)