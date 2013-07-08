(ns ajfx.drafts)



; 
(damp.ekeko/ekeko* [?advice ?type] (w/advice ?advice)
                      (equals ?type (.getClass ?advice)))