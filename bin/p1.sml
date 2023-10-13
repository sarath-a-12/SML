(*Question 1*)

datatype 'a tree = null
                 | node of 'a * 'a tree * 'a tree  (*'a Tree is defined by a
                 node containing 'a,and two children each of which are
                 recursively defined to be 'a trees*)

(*Question 2*)

(*mapT : ('a -> 'b) -> 'a Tree -> 'b Tree*)

(*f is the mapping function. null maps to null. otherwise apply f to the value
* on node and recursively map the left and right subtrees*)

fun mapT f null = null
  | mapT f (node(va,lef,righ)) = node(f va ,(mapT f lef),(mapT f righ))


(* Question 3 *)

(* 
* preorder : 'a Tree -> 'a list        val->left->right
* inorder :  'a Tree -> 'a list        left->val->right
* postorder: 'a Tree -> 'a list        left->right->val
* *)


(* use the above said definitions to concat lists in the desired manner *)
fun inorder null = []
  | inorder (node(va,lef,rig)) = ((inorder (lef))@[va]@(inorder (rig)))

fun preorder null = []
  | preorder (node(va,lef,rig)) = ([va]@(preorder (lef))@(preorder (rig)))


fun postorder null = []
  | postorder (node(va,lef,rig)) = ((postorder (lef))@(postorder (rig))@[va])

(*Question 4*)

(*
* foldTree ('a * 'sumarry * 'summary -> 'sumarry) -> 'summary -> 'a tree -> 'summary
* foldtree takes in a function, which takes in 'a , and two
* summaries(corresponding to left and right children) , an initial summary and
* a_tree to get the final summary
*)

fun foldTree f summary null = summary
  | foldTree f summary (node(va, lef, rig)) = f(va,foldTree f summary lef, foldTree f summary rig) 

(*Question 5*)
(*Inorder in terms of foldTree*)

(*inorderfold : 'a tree -> 'a list*)
(*f will take in a value and two lists and produce l1 concat v concat l2, where
* l1 and l2 corresponds to the inorder traversals of left and right subtrees
* respectively*)
fun f (v,l1,l2) = l1 @ [v] @ l2

(*initial summary empty list use the above defined f as the summarising function*)
fun inorderfold a_tree = foldTree f [] a_tree 


(*Question 6*)

(*follows from the definition of rotation, corner case checked by pattern
* matching*)

fun rot_cw (node(a,node(b,t1,t2),t3)) = node(b,t1,node(a,t2,t3))
  | rot_cw t = t

