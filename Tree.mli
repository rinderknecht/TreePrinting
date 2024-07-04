(* This module is a DSL for building textual representations of
   Catalan trees (general trees).

   The underlying design principle of this module is that the exported
   functions always print a valid tree, that is, there is a root and
   all the subtrees (children) are accounted for in the layout. *)

(* Vendor dependencies *)

module Region    = SourceLoc.Region
module Sequences = Utilities.Sequences

(* STATE *)

type state

val mk_state :
  ?buffer:Buffer.t ->
  regions:bool ->
  layout:bool ->
  offsets:bool ->
  [ `Byte | `Point ] ->
  state

val to_buffer : state -> Buffer.t

(* ROOTS (labels at the root node) *)

type root = string

(* PRINTERS *)

(* A printer is a function of type ['a printer] that takes a stateful
   [state] and prints its second argument, that is, commits it to the
   state as a formatted string. *)

type 'a printer = state -> 'a -> unit

(* PRINTING GENERAL TREES *)

(* A child is an optional value: if [None] there is no actual
   child. This is used to build lists of children and only care
   afterwards if any is missing, by discarding the [None] values. If
   present with a [Some print] value, the function [print] is a
   printer specialised for the child (see type ['a printer] above),
   which enables building lists of such values and then iterate
   through them to print all the subtrees. *)

type child = (state -> unit) option

(* The call [make_tree ?region state root children] prints to [state]
   a root [root] and subtrees [children]. The latter is a list of
   optional values, with the interpretation of [None] as meaning "no
   subtree printed" (see MAKING SUBTREES above). *)

val make_tree :
  ?region:Region.t -> state -> root -> child list -> unit

val make : (* Alias of [make_tree] *)
  ?region:Region.t -> state -> root -> child list -> unit

(* PRINTING NODES (trees without children) *)

(* The call [make_node ?region state item] prints a leaf of the tree
   with or without its region in compact form. The label of the leaf
   has type [string]. This fonction can be used when printing nodes
   whose aim is to guide the interpretation, but do not correspond to
   an actual node in the tree, for example "<cst>", or "<statements>"
   (in other word, metadata nodes). *)

val make_node : ?region:Region.t -> string printer

(* PRINTING UNARY TREES (trees with exactly one child) *)

val make_unary :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer (* printer for the unique child *)
  -> 'a         (* unique child *)
  -> unit

(* MAKING CHILDREN (subtrees) *)

(* Making subtrees (children) from
     * general values ([mk_child]),
     * optional values ([mk_child_opt]).
   The type of a subtree ("child") is ['a option], with the
   interpretation that [None] means "no subtree printed". *)

val mk_child     : 'a printer -> 'a        -> child
val mk_child_opt : 'a printer -> 'a option -> child

(* Making lists of children from linear data structures.

   If the argument [root] is given, then the result is a singleton
   list containing a tree with that root, and whose children come from
   the linear data structure (list, sequence). *)

val mk_children_list :
  'a printer -> ?root:root -> 'a list -> child list

val mk_children_nsepseq :
  'a printer -> ?root:root -> ('a,_) Sequences.nsepseq -> child list

val mk_children_nsepseq_opt :
  'a printer -> ?root:root -> ('a,_) Sequences.nsepseq option -> child list

val mk_children_sepseq :
  'a printer -> ?root:root -> ('a,_) Sequences.sepseq -> child list

val mk_children_ne_list :
  'a printer -> ?root:root -> 'a Sequences.nseq -> child list

val mk_children_sep_or_term :
  'a printer -> ?root:root -> ('a,_) Sequences.sep_or_term -> child list

val mk_children_nsep_or_term :
  'a printer -> ?root:root -> ('a,_) Sequences.nsep_or_term -> child list

val mk_children_nsep_or_pref :
  'a printer -> ?root:root -> ('a,_) Sequences.nsep_or_pref -> child list


(* PRINTING LISTS AND SEQUENCES *)

(* The call [of_list ?region state ?root print list] prints a tree of
   root [root] and whose children correspond to [list]. If there are
   no children, the tree is a node. This can also happen with
   [of_sepseq], but neither [of_nsepseq] nor [of_ne_list]. *)

val of_list :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer (* For each child. *)
  -> 'a list    (* To make children from. *)
  -> unit

val of_nsepseq :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer
  -> ('a,_) Sequences.nsepseq
  -> unit

val of_sepseq :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer
  -> ('a,_) Sequences.sepseq
  -> unit

val of_ne_list :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer
  -> 'a Sequences.nseq
  -> unit

val of_sep_or_term :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer
  -> ('a,_) Sequences.sep_or_term
  -> unit

val of_nsep_or_term :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer
  -> ('a,_) Sequences.nsep_or_term
  -> unit

val of_nsep_or_pref :
  ?region:Region.t
  -> state
  -> root
  -> 'a printer
  -> ('a,_) Sequences.nsep_or_pref
  -> unit
