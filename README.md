
# Haskell Coursework

This is my submission for the Haskell Coursework for COMP0002. Our tasks involved designing a datatype that represents the mathematical concept of a finite set of elements of the same type and also support the required functions over sets.


## Introduction

I chose to develope a datatype based on the standard Binary Search Tree since it is a more efficient implementation when compared to a List or Linked List. This is due to its property of storing a Node with a value and also storing a left and right branches with values smaller and greater than the Node respectively.

Datatype: Set a = Null | Node (Set a) a (Set a)

Datatype Examples:
1) Node 3 4 5
2) Node Null 7 9
3) Node (Node 3 5 Null) 6 8
4) Node (Node (Node Null 3 4) 5 (Node Null 6 Null)) 8 (Node Null 9 Null)
## Running The Program

First, using the terminal initiate the GHCi package by simply entering 'ghci' in the terminal (Make sure you are in the same directory as the haskell file). 

Next, use ':l Coursework.hs' to load up the haskell file. 

Finally, you use the Datatype examples above to make Binary Search Trees and then perform the operations provided in the file. 

Examples:
1) fromList [2,4,6,1,8,6,9]
2) insert Tree 5
3) union Tree1 Tree2
4) powerSet Tree

(Tree, Tree1 and Tree2 are Sets of the Datatype provided)