(ns
  ^{:doc "Extra Soot relations for use in Ekeko queries"
    :author "Tim Molderez" }
  ekeko-ajfx.soot
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use [damp.ekeko logic]
        [damp.ekeko])
  (:import [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [org.aspectj.lang Signature]
    [java.lang Integer]
    [org.eclipse.jdt.core IJavaElement ITypeHierarchy IType IPackageFragment IClassFile ICompilationUnit
     IJavaProject WorkingCopyOwner IMethod]
    [org.eclipse.jdt.core.dom Expression IVariableBinding ASTParser AST IBinding Type TypeDeclaration 
     QualifiedName SimpleName ITypeBinding MethodDeclaration
     MethodInvocation ClassInstanceCreation SuperConstructorInvocation SuperMethodInvocation
     SuperFieldAccess FieldAccess ConstructorInvocation ASTNode ASTNode$NodeList CompilationUnit]
    [org.aspectj.weaver.patterns Pointcut AndPointcut]))

(defn soot|method-name
  "Relate a soot method to its name" [?method ?name]
  (l/all
    (jsoot/soot :method ?method)
    (equals ?name (.getName ?method))))

(defn method-signature [method]
  "Converts a signature from a SootMethod to the format used in join point shadows" 
  (let [split (clojure.string/split (-> method .getSubSignature) #" ")
        cls (-> method .getDeclaringClass)
        meth (clojure.string/replace (second split) #"," ", ")]
    (str (first split) " " cls "." meth)))

(defn 
  soot|method-sig
  "Relate a SootMethod to its signature (String)"
  [?method ?sig]
  (l/all
    (jsoot/soot :method ?method)
    (equals ?sig (method-signature ?method))))

(defn 
  soot|method-sig-full
  "Relate a SootMethod to its complete signature (String)"
  [?method ?sig]
  (l/all
    (jsoot/soot :method ?method)
    (equals ?sig (-> ?method .getSignature))))

(defn strip-advice-id [sig]
  "Given the signature of an advice, strip the internally generated ID number.."
  (let [end-id (-> sig (.lastIndexOf "("))
        substr (subs sig 0 end-id)
        begin-id (-> substr (.lastIndexOf "$"))]
    (if (= begin-id -1)
      sig
      (str 
        (subs sig 0 begin-id)
        (subs sig end-id)))))

(defn 
  soot|advice-sig-full
  "Relate a SootMethod of an advice to its complete signature (disregarding the ID number (?) that's in there)"
  [?method ?sig]
  (l/all
    (jsoot/soot :method ?method)
    (equals (strip-advice-id ?sig) (strip-advice-id (-> ?method .getSignature)))))

(defn get-all-advice []
  "Retrieve all advice of Ekeko-enabled projects (as BcelAdvice instances)"
  (set (ekeko [?adv] (w/advice ?adv))))

(defn get-all-advice-and-shadows []
  "Retrieve all advice and their corresponding join point shadows (as a list of BcelAdvice,ProgramElement pairs)" 
  (set (ekeko [?adv-soot ?meth]
         (l/fresh [?adv]
           (w/advice ?adv)
           (w/advice-shadow ?adv ?meth)
           (ajsoot/advice-soot|method ?adv ?adv-soot)))))

(defn get-all-advice-bodies []
  "Retrieve all advice bodies of Ekeko+Soot-enabled projects (as SootMethod instances)" 
  (set (ekeko [?method]
         (l/fresh [?advice]
           (w/advice ?advice)
           (ajsoot/advice-soot|method ?advice ?method)))))

(defn get-all-bodies []
  "Retrieve all method and advice bodies of Ekeko+Soot-enabled projects (as SootMethod instances)"
  (ekeko [?method]
         (l/fresh []
           (jsoot/soot :method ?method))))

(defn get-method-from-shadow [shadow]
  "Try to determine the SootMethod described in a join point shadow (as obtained via w/advice-shadow)
   (!! This doesn't work very reliably.. only the .toString value of a shadow seems to be of any use, but it often doesn't contain enough information..)"
  (let [str (-> shadow .toString)
        open (-> str (.indexOf "("))
        close (-> str (.lastIndexOf ")"))
        ?sig (subs str (inc open) close)
        query (ekeko [?m] (soot|method-sig ?m ?sig))]
    (first (first query))))

(defn write-advice-shadows []
  "Write all advice - join point shadow pairs to a file.."
  (let [pairs (for [x (get-all-advice-and-shadows)]
                (try
                  [(-> (first x) .getSignature)
                   (-> (get-method-from-shadow (second x)) .getSignature)]
                  (catch Exception e (println "!!! No body found for shadow:" (second x)))))
        filtered (remove (fn [x] (= x nil))
                   pairs)]
    (spit "advice-shadow-pairs.txt" (pr-str (into [] filtered)))))

(defn read-advice-shadows []
  "Read the advice - join point shadow pairs produced by (write-advice-shadows)"
  (let [pairs (load-file "advice-shadow-pairs.txt")]
    (into [] (set (for [x pairs]
                  [(first (first (ekeko [?m] (soot|advice-sig-full ?m (first x)))))
                   (first (first (ekeko [?m] (soot|method-sig-full ?m (second x)))))])))))