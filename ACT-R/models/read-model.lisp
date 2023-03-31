(define-model read

; :lf parameter is latency factor (set in fan model as .63)
; :mas parameter is max associative strength/turns on spreading activation
; (set as 1.6 in fan model)
; :act parameter if t (nil if off) causes the declarative memory
; system to print the details of the activation computations
; that occur during a retrieval request in the trace.
; ;ans is instantaneous noise parameter (noise varying trial by trial)
; :rt parameter sets retrieval threshold

  (sgp :v t :act t :esc t :lf .63
    :mas 1.6 :bll 0.5 :ans 0.5 :rt -2)
  (sgp :style-warnings nil)

  (chunk-type association arg1 arg2)
  (chunk-type meaning word)
  (chunk-type task state cue response target)

  (add-dm
    (goal isa task state start) (detect-target isa chunk)
    (attend isa chunk) (retrieve-meaning isa chunk)
    (retrieve-associate isa chunk) (respond isa chunk) (find-target isa chunk)
    (end isa chunk)
   (p1 ISA association arg1 adventure arg2 trip)
   (p2 ISA association arg1 adventure arg2 outdoors)
   (p3 ISA association arg1 adventure arg2 bike)
   (p4 ISA association arg1 bagel arg2 butter)
   (p5 ISA association arg1 bagel arg2 creamcheese)
   (p6 ISA association arg1 bagel arg2 everything)
   (p7 ISA association arg1 candle arg2 wick)
   (p8 ISA association arg1 candle arg2 smell)
   (p9 ISA association arg1 candle arg2 lighter)
   (p10 ISA association arg1 direction arg2 arrow)
   (p11 ISA association arg1 direction arg2 up)
   (p12 ISA association arg1 direction arg2 ask)
   (p13 ISA association arg1 home arg2 sick)
   (p14 ISA association arg1 home arg2 house)
   (p15 ISA association arg1 home arg2 garage)
   (p16 ISA association arg1 octopus arg2 ocean)
   (p17 ISA association arg1 octopus arg2 sea)
   (p18 ISA association arg1 octopus arg2 jellyfish)
   (p19 ISA association arg1 potato arg2 mash)
   (p20 ISA association arg1 potato arg2 root)
   (p21 ISA association arg1 potato arg2 tuber)
   (p22 ISA association arg1 stack arg2 build)
   (p23 ISA association arg1 stack arg2 pancakes)
   (p24 ISA association arg1 stack arg2 paper)
   (p25 ISA association arg1 wreck arg2 ship)
   (p26 ISA association arg1 wreck arg2 ball)
   (p27 ISA association arg1 wreck arg2 construction)

   (adventure ISA meaning word "adventure")
   (trip ISA meaning word "trip")
   (outdoors ISA meaning word "outdoors")
   (bike ISA meaning word "bike")

   (bagel ISA meaning word "bagel")
   (butter ISA meaning word "butter")
   (creamcheese ISA meaning word "creamcheese")
   (everything ISA meaning word "everything")

   (candle ISA meaning word "candle")
   (wick ISA meaning word "wick")
   (smell ISA meaning word "smell")
   (lighter ISA meaning word "lighter")

   (direction ISA meaning word "direction")
   (arrow ISA meaning word "arrow")
   (up ISA meaning word "up")
   (ask ISA meaning word "ask")

   (home ISA meaning word "home")
   (sick ISA meaning word "sick")
   (garage ISA meaning word "garage")
   (house ISA meaning word "house")

   (octopus ISA meaning word "octopus")
   (ocean ISA meaning word "ocean")
   (sea ISA meaning word "sea")
   (jellyfish ISA meaning word "jellyfish")

   (potato ISA meaning word "potato")
   (mash ISA meaning word "mash")
   (root ISA meaning word "root")
   (tuber ISA meaning word "tuber")

   (stack ISA meaning word "stack")
   (build ISA meaning word "build")
   (pancakes ISA meaning word "pancakes")
   (paper ISA meaning word "paper")

   (wreck ISA meaning word "wreck")
   (ship ISA meaning word "ship")
   (ball ISA meaning word "ball")
   (construction ISA meaning word "construction")
   )


   ; first half of model will attend and encode cue -- if another
   ; item is detected (visual location nil) then it will continue to encode a paired
   ; if there is no other item on the screen the model will retrieve an associate

   ; need to make sure find cue specifies the top word though if 2 on screen :)

   (P find-cue
        =goal>
          isa         task
          state       start
        ?visual-location>
          buffer      unrequested
      ==>
        =goal>
          state       find-location
        +visual-location>
          isa         visual-location
          screen-y    lowest
      )

    (P attend-cue-location
        =goal>
            state     find-location
        =visual-location>

        ?visual-location>
            buffer    requested
        ?visual>
            state     free
      ==>
        =goal>
            state       attend
        +visual>
            cmd       move-attention
            screen-pos  =visual-location)

    (P retrieve-cue-meaning
        =goal>
            state     attend
        =visual>
            isa       visual-object
            value     =word
      ==>
        =goal>
            state     retrieve-meaning
        +retrieval>
            isa       meaning
            word      =word)

    (P encode-cue
        =goal>
            state     retrieve-meaning
            cue       nil
        =retrieval>

      ==>
        =goal>
            state     detect-target
            cue       =retrieval
        +imaginal>
            isa       association
            arg1      =retrieval
        )

; this is where model branches off:

    (P detect-target
        =goal>
            state     detect-target
        =imaginal>
            isa       association
      ==>
        =imaginal>
        +visual-location>
            :attended    nil
        =goal>
            state       find-location
      )

    (P attend-target-location
        =goal>
            state     find-location
        =imaginal>
        =visual-location>

        ?visual-location>
            buffer    requested
        ?visual>
            state     free
      ==>
        =goal>
            state       attend
        =imaginal>
        +visual>
            cmd       move-attention
            screen-pos  =visual-location)

    (P retrieve-targ-meaning
        =goal>
            state     attend
        =imaginal>
        =visual>
            isa       visual-object
            value     =word
      ==>
        =goal>
            state     retrieve-meaning
        =imaginal>
        +retrieval>
            isa       meaning
            word      =word)

  (P encode-target
       =goal>
           state     retrieve-meaning
         - cue       nil
       =retrieval>

       =imaginal>
           isa       association
         - arg1      nil
     ==>
       =goal>
           state     end
           target     =retrieval
       =imaginal>
           arg2      =retrieval
         )
;for a study pair this would fire to reset the model

 (P end-study
      =goal>
          state     end
          response  nil
    ==>
      =goal>
          isa       task
          state     start
          cue       nil
          response  nil
          target    nil
      -imaginal>
      +visual>
          cmd       clear)
; now here is where model would retrieve an associate if no new visual

    (P no-target
        =goal>
            state     find-location
          - cue       nil
        ?visual-location>
            buffer    failure
      ==>
        =goal>
            state     retrieve-associate
        )

    (P retrieve-from-cue
        =goal>
            state     retrieve-associate
        =imaginal>
            isa       association
            arg1      =cue
        ?retrieval>
            state     free
            buffer    empty
      ==>
        =goal>
            state     respond
        =imaginal>
        +retrieval>
            isa       association
            arg1      =cue)

    (P respond
        =goal>
            state     respond
            cue       =cue
        =retrieval>
            isa       association
            arg1      =cue
            arg2      =response
        =imaginal>
            isa       association

        ?manual>
            state     free
      ==>
        =imaginal>
        =goal>
            state     find-target
            cue       =cue
            response  =response
        +manual>
            cmd       press-key
            key       space
        )

    (P find-target
        =goal>
            state     find-target
        =visual-location>
        ?visual>
            state     free
      ==>
        =goal>
            state     attend
        +visual>
            cmd       move-attention
            screen-pos  =visual-location
        )

; retrieve-meaning fires again but doesnt encode cue because cue slot has value


    (P correct
            =goal>
                ISA       task
                state     end
                cue       =cue
                response  =response
                target    =response
            =imaginal>

            ?manual>
                state     free
            ?visual>
                state     free
          ==>
            +manual>
                cmd       press-key
                key       "f"
            =goal>
                isa       task
                state     start
                cue       nil
                response  nil
                target    nil
            -imaginal>
            +visual>
                cmd       clear
            !output!      correct
            )

    (P incorrect
            =goal>
                ISA       task
                state     end
                cue       =cue
                response  =response
              - target    =response
            ?manual>
                state     free
          ==>
            +manual>
                cmd       press-key
                key       "j"
            =goal>
                isa       task
                state     start
                cue       nil
                response  nil
                target    nil
            -imaginal>
            +visual>
                cmd       clear
            !output!      incorrect
                  )

      (goal-focus goal)

  )
