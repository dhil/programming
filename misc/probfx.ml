(* A reconstruction of "Effect Handlers for Programmable Inference" by Nguyen et al. (2023) *)

module Utils = struct
  let neg_inf = -. 1. /. 0.
  let pos_inf = 1. /. 0.

  let inv_erfc : float -> float
    = fun y ->
    if Float.compare y 0. <= 0 then pos_inf
    else if Float.compare y 2. >= 1 then neg_inf
    else
      let x = 1.0 -. y in
      let w = -. log ((1.0 -. x) *. (1.0 +. x)) in
      let p =
        if w < 5.0 then
          let w = w -. 2.5 in
          let p = 2.81022636e-08 in
          let p = 3.43273939e-07 +. p *. w in
          let p = -.3.5233877e-06 +. p *. w in
          let p = -.4.39150654e-06 +. p *. w in
          let p = 0.00021858087 +. p *. w in
          let p = -.0.00125372503 +. p *. w in
          let p = -.0.00417768164 +. p *. w in
          let p = 0.246640727 +. p *. w in
          1.50140941 +. p *. w
        else
          let w = sqrt w -. 3.0 in
          let p = -.0.000200214257 in
          let p = 0.000100950558 +. p *. w in
          let p = 0.00134934322 +. p *. w in
          let p = -.0.00367342844 +. p *. w in
          let p = 0.00573950773 +. p *. w in
          let p = -.0.0076224613 +. p *. w in
          let p = 0.00943887047 +. p *. w in
          let p = 1.00167406 +. p *. w in
          2.83297682 +. p *. w
      in
      p *. x

  let sqrt_2 = Float.sqrt 2.
  let sqrt_2_pi = Float.sqrt (2. *. Float.pi)
end

module Dist = struct
  type 'a t = Bernoulli : float -> bool t
            | Normal : float * float -> float t
  (* TODO add more distributions here *)

  let bernoulli : float -> bool t
    = fun p ->
    assert (Float.compare p 0. >= 0 && Float.compare p 1. <= 0);
    Bernoulli p

  let normal : float -> float -> float t
    = fun mu sigma ->
    assert (Float.compare sigma 0. >= 0);
    Normal (mu, sigma)

  let draw : type a. float -> a t -> a
    = fun r d ->
    match d with
    | Bernoulli p -> r <= p
    | Normal (mu, sigma) ->
       (-. Utils.inv_erfc (2. *. r)) *. (Utils.sqrt_2 *. sigma) +. mu

  let log_prob : type a. a -> a t -> float
    = fun x d ->
    match d with
    | Bernoulli p ->
       if x then log p else log (1. -. p)
    | Normal (mu, sigma) ->
       let x_mu = x -. mu in
       -. (x_mu *. x_mu /. (2. *. (sigma *. sigma))) -. log Utils.sqrt_2_pi -. log sigma
end

(* Metropolis-Hastings inference *)
module Addr = struct
  type t = int
  let make =
    let next = ref (-1) in
    fun () ->
    incr next; !next
end

module AddrMap = Map.Make (struct
                     type t = Addr.t
                     let compare = Int.compare
                   end)

module Trace = struct
  type t = float AddrMap.t

  let try_insert : Addr.t -> float -> t ref -> float
    = fun addr r tr ->
    (* TODO(dhil): Optimise with a bespoke map *)
    match AddrMap.find_opt addr !tr with
    | None -> tr := AddrMap.add addr r !tr; r
    | Some r' -> r'

  let empty : t = AddrMap.empty

  let pp_trace : Format.formatter -> t -> unit
    = fun fmt tr ->
    let size = AddrMap.cardinal tr in
    let cur = ref 0 in
    Format.fprintf fmt "{";
    AddrMap.iter (fun addr value ->
        Format.fprintf fmt "%d -> %f" addr value;
        incr cur;
        if !cur < size then Format.fprintf fmt ", ") tr;
    Format.fprintf fmt "}"
end
#install_printer Trace.pp_trace;;

(* Type container for type of random values; useful for type
   ascription on functor arguments. *)
module type R = sig
  type t
end

(* Effect signatures *)
module type SAMPLE = sig
  type a (* type of random values *)

  type _ Effect.t += Sample : a Dist.t * Addr.t -> a Effect.t
end

module Sample = struct
  module Make(T : R) : SAMPLE with type a = T.t = struct
    type a = T.t (* type of random values *)

    type _ Effect.t += Sample : a Dist.t * Addr.t -> a Effect.t
  end
end

module type OBSERVE = sig
  type a (* type of random values *)

  type _ Effect.t += Observe : a Dist.t * Addr.t * a -> a Effect.t
end

module Observe = struct
  module Make(T : R) : OBSERVE with type a = T.t = struct
    type a = T.t (* type of random values *)

    type _ Effect.t += Observe : a Dist.t * Addr.t * a -> a Effect.t
  end
end

(* Capability signatures *)
type 'a sample_cap = (module SAMPLE with type a = 'a)
type 'a observe_cap = (module OBSERVE with type a = 'a)

(* TODO can probably encode a lightweight effect type / capability
   type here too. *)
type ('c, 'a) model = Model of ('c -> 'a)

(* Metropolis-Hastings inference *)
let reuse_trace (type a) (type b) : a sample_cap -> Trace.t -> (unit, b) model -> b * Trace.t
  = fun (module S) tr0 (Model m) ->
  let tr = ref tr0 in
  match m () with
  | ans -> (ans, !tr)
  | effect (S.Sample (d, addr)), k ->
     let r = Trace.try_insert addr (Random.float 1.0) tr in
     Effect.Deep.continue k (Dist.draw r d)

(* Inference operations *)
module type PROPOSE = sig
  type a (* type of random values *)
  type w = float (* type of weights *)

  type _ Effect.t +=
     | Propose : Trace.t -> Trace.t Effect.t
     | Accept : ((a * (w * Trace.t)) * (a * (w * Trace.t))) -> (a * (w * Trace.t)) Effect.t
end

module Propose = struct
  module Make(T : R) : PROPOSE with type a = T.t = struct
    type a = T.t
    type w = float

  type _ Effect.t +=
     | Propose : Trace.t -> Trace.t Effect.t
     | Accept : ((a * (w * Trace.t)) * (a * (w * Trace.t))) -> (a * (w * Trace.t)) Effect.t
  end
end

(* Further capability signature *)
type 'a propose_cap = (module PROPOSE with type a = 'a)

module MetropolisHastingsCapabilities = struct
  module Make(M : sig
               type t
               type ans
             end) = struct
    module Observe : OBSERVE with type a = M.t = Observe.Make(M)
    module Sample : SAMPLE with type a = M.t = Sample.Make(M)
    module Propose : PROPOSE with type a = M.ans = Propose.Make(struct type t = M.ans end)
  end
end

type ('c, 'w, 'a) model_exec = ModelExec of ('c -> Trace.t -> ('c, 'a) model -> ('a * ('w * Trace.t)))

let mh (type a) (type b) : (b propose_cap * a sample_cap * a observe_cap) ->
                           int ->
                           Trace.t ->
                           (a sample_cap * a observe_cap, float, b) model_exec ->
                           (a sample_cap * a observe_cap, b) model ->
                           (b * (float * Trace.t)) list
  = fun ((module P),(module S),(module O)) n tr0 (ModelExec exec) m ->
  let rec mh_step i chain =
    if i < n
    then let (x, (w, tr)) = List.hd chain in
         let tr' = Effect.perform (P.Propose tr) in
         let (x', (w', tr'')) = exec ((module S), (module O)) tr' m in
         let node = Effect.perform (P.Accept ((x, (w, tr)), (x', (w', tr'')))) in
         mh_step (i + 1) (node :: chain)
    else chain
  in
  let node0 = exec ((module S), (module O)) tr0 m in
  mh_step 0 [node0]

let handle_propose (type a) (type b) (type c) : (b propose_cap * a sample_cap * a observe_cap) -> (b propose_cap * a sample_cap * a observe_cap, c) model -> c
  = fun (((module P), (module S), (module O)) as caps) (Model m) ->
  match m caps with
  | ans -> ans
  | effect P.Propose tr, k ->
     let tr' = AddrMap.map (fun _ -> Random.float 1.0) tr in
     Effect.Deep.continue k tr'
  | effect P.Accept (((_, (w, _)) as r), ((_, (w', _)) as r')), k ->
     let ratio = w' -. w in
     let u = Random.float 1.0 in
     Effect.Deep.continue k (if (exp ratio) >= u then r' else r)

let likelihood (type a) (type b) : a observe_cap -> (a observe_cap, b) model -> (b * float)
  = fun (module O) (Model m) ->
  let w = ref 0. in
  match m (module O) with
  | ans -> (ans, !w)
  | effect O.Observe (d, addr, y), k ->
     w := !w +. Dist.log_prob y d;
     Effect.Deep.continue k y

let exec_model_im (type a) (type b) : (a sample_cap * a observe_cap, float, b) model_exec
  = ModelExec (fun ((module S), (module O)) tr (Model m) ->
        let ((x, w), tr') =
          reuse_trace (module S)
            tr
            (Model (fun () -> (* Some capability polymorphism is desired here *)
                 likelihood (module O)
                   (Model (fun (module O) -> m ((module S), (module O))))))
        in
        (x, (w, tr')))

let im (type a) (type b) : int -> (a sample_cap * a observe_cap, b) model -> (b * (float * Trace.t)) list
  = fun n m ->
  let module Caps = MetropolisHastingsCapabilities.Make(struct
                        type t = a
                        type ans = b
                      end)
  in
  handle_propose
    ((module Caps.Propose), (module Caps.Sample), (module Caps.Observe))
    (Model (fun caps -> mh caps n Trace.empty exec_model_im m))

let _ = Random.init 42 (* for deterministic results *)

let demo () =
  let addr = Addr.make () in
  let lin_regr : float -> float -> (float sample_cap * float observe_cap, float * float) model
    = fun x y ->
    Model (fun ((module S), (module O)) ->
        let m = Effect.perform (S.Sample (Dist.normal 0. 3., addr)) in
        let c = Effect.perform (S.Sample (Dist.normal 0. 2., addr)) in
        ignore (Effect.perform (O.Observe (Dist.normal (m *. x +. c) 1., addr, y)));
        (m, c))
  in
  im 10 (lin_regr 0.5 0.1)
