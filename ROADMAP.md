# Roadmap / feature-ideas 

* create more operators for manipulating rhythms ('a list)
* come up with way of laying out beats in a simple way, and mapping them to a more complex rhythmic structure
* brainstorm on if Lwt_react semantics of delaying events should be used 
  * or if it's better to stick to a grid of events
* ratchet; brainstorm on if it could be wanted to trigger a short sequence of events that are not later overruled 
  by next event in baseline event-seq
* make example where a more complex note type is used, which bears info about how to modulate different values
  * or about if ratchet should be triggered 
  * etc.
* make example where a specific rhythm is sped up (e.g. by a multiplier), but other rhythms are not
* should Beat's be able to be synchronized somehow?
  * e.g. 
    * one slow beat is quantized to fit on top of fast beat?
* make example with more complex generative rhythm
* make example that uses notty for both rhythm modification + colored screen output
  * make sure that `fry` lib doesn't end up depending on notty this way - guess not
* make `fry` not depend on lwt.unix - Fry.Beat.Make should get passed a sleep proc
  * possibly make a specialized version of `fry` that depends on lwt.unix?
* guarantee the timing of events in Fry.Beat.Make relative to start-time
  * < will e.g. allow synchronization with other music
* make Beat.Make useable for dynamic creation, 
  * which means some way to 'GC'/stop the beat
    * e.g. including a 'stop' proc, that: 
      * stops lwt-loop
      * stops 's' and 'e'
    * @idea; there could also be a 'stop ~when' where 'when' specifies a timeout for GC
  * I guess there should also be a module type so one can create it as a first-class module
    * try out if a 'make'-function makes more sense syntactically 
* create example with dynamically modified and created rhythms
