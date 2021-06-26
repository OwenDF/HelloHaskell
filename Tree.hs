data Tree =
  Leaf |
  Node Int Tree Tree deriving Show

treeDepth Leaf = 0
treeDepth (Node _ left right) =
  1 + max (treeDepth left) (treeDepth right)

treeSum Leaf = 0
treeSum (Node value left right) =
  value + (treeSum left) + (treeSum right)

validateNode val comp (Leaf) = True
validateNode val comp (Node compval left right) =
  comp val compval

validateTree Leaf = True
validateTree (Node value left right) = 
  (validateTree left) &&
  (validateTree right) &&
  validateNode value (>) left &&
  validateNode value (<) right

appendTree Leaf val = (Node val Leaf Leaf)
appendTree (Node nodeVal left right) val
  | nodeVal == val = Node nodeVal left right
  | nodeVal < val = Node nodeVal left (appendTree right val)
  | nodeVal > val = Node nodeVal (appendTree left val) right
