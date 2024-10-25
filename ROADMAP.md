# Roadmap / feature-ideas 

* make examples for using 'Wavelet' module (prev 'Envelope') for animations (e.g. CSS + Tyxml)
* idea for Envelope; should enable creation of a 'global envelope' that morphs between envelopes 
  (constructive inteference)
* @idea; allow user to specify that beat should have limited lifetime?
  * or let user control lifetime themselves, by calling a 'stop' proc?
  * lifetimes can be:
    * amount of events shot 
    * timeout
    * till some given thread resolves
* Beat.Make has an 'e' and 's' - but divide speed operates only on 'e'
  * should Beat.Make include a function to derive full modules with divided speed?
  * should Beat.Make expose input bpm_s ?
* come up with more ways of laying out beats in a simple way, 
  * and then mapping them to a more complex rhythmic structure
    * < check out 'bottom tsh' language for inspiration
* brainstorm on if Lwt_react semantics of delaying events should be used 
  * or if it's better to stick to a grid of events
* ratchet; brainstorm on if it could be wanted to trigger a short sequence of 
  events that are not later overruled by next event in baseline event-seq
  * is this maybe just explicit usage of E.switch?
    * try to make an example with this usecase
* make example where a specific rhythm is sped up dynamically (e.g. by an integer multiplier), 
  * but other rhythms are not
* should Beat's be able to be synchronized somehow?
  * e.g. 
    * one slow beat is quantized to fit on top of fast beat?
      * (i.e. for beats that are not derived from eachother)
* make example with more complex generative rhythm
  * involving e.g.:
    * dynamic change of rhythms based on randomness
    * some looping tendencies, so it's not all randomness
    * dynamically changing beat bpm's
      * let one rhythm change the bpm of another with envelope 
        * .. i.e. effect rolling off over time, after note has shot
      * < @idea; maybe this becomes own example 
* make example that uses notty for complex interactive rhythmic modification/creation
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

