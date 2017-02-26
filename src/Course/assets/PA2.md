# Data Structures
## Week 1
1. Arrays and Linked Lists
    a. Array: contiguous area of memory cosisting of equal-size elements indexed by contiguous integers(*note: the size is predetermined*).
        - Key Characters: Constant-time access
        - Ordering: 这就是list有序的来源，在内存中构建的时候他就是有序的，有起点与终点的区别。
        - Times for common operations: Constant time to add/remove at the end; linear time to add/remove at arbitrary location.
    b. Singly-Linked Lists: lists of node which contains a key and next pointer
        - API: PushFront(key) -> add front; PopFront() -> remove front;
        - Times for common operations:
            - no tail: PushFront -> O(1); PpFront -> O(1); PushBack -> O(n); PopBack -> O(n); AddBefore -> O(n); AddAfter -> O(1)
            - with tail: PushFront -> O(1); PpFront -> O(1); PushBack -> O(1); PopBack -> O(1); AddBefore -> O(n); AddAfter -> O(1)
    c. Doubly-Linked Lists: instead a pointer pointing just one direction, this node has two direction
        - Times: all goes to O(1)
2. Stack and Queues
    a. Stack: Abstract data type with the following operations:
            - Push(key): adds key to collection
            - Key Top(): returns most recently-added key
            - Key Pop(): removes and returns most recently-added key
            - Boolean Empty(): are there any element
        - applications: balanced brackets
        - Stacks can be implemented with either an array or a linked list.
        - Each stack operation is O(1)
        - LIFO: last in, first in
        - *In haskell: it's just List*
    b. Queus: Abstract data type with following operations:
            - Enqueue(Key): adds key to collection
            - Key Dequeue(): removes and returns least recently-added key(FIFO)
            - Bollean Empty(): are there any element?
        - Queues can be implemented with either a linked list (with tail pointer) or an array
        - Each operation is O(1)
        - *In Haskell, we can use Data.Sequeue*
3. Trees
    a. def: Empty or a node with a key and a list of child tree (recursively definition)
    b. Examples: Binary Search Tree/Syntax Tree
    c. Terminology: 1) Root 2) Child and Parents 3) Ancestors and Descendants and Sibling 4) Leaf: node has no child 5) interior node: non-leaf 6) Level: 1+num edges between root and node 7) Height: maximum depth of subtree node and forefather node 8) Forest: collection of trees
    d. Tree Traversal: often we want visit the nodes of a tree in a particular order.
        - Two main methods:
            1) *Depth first*:we completely traverse one sub-tree before exploring a sibling sub-tree;
                - pre-order VS in-order VS post-order
                - post-order similar to stack, FILO
            2)*Breadth-first*:we traverse all nodes at one level before progressing to the next level.
                - use queus to store child tree and travel all same level nodes' keys
4. Dynamic Arrays and Amortized Analysis
    a. Problem: static arrays are static! and semi-solution: dynameically-allocated arrays, but we might not know max size when allocating an array.
    b. Solution: dynamic arrays (also called resizable arrays) -> store a pointer to a dynamically allocated array, and replace it with a newly-allocated array as need.
    c. Def: Abstract data type with following operations:
        - Get(i): returns element at location i*
        - Set(i, val): sets element i to val*
        - PushBack(val): Adds val to the end
        - Remove()
        - Size()
    d. Implementation: 1) arr: dynameically-allocated array 2) capacity: size of the dynameically-allocated array 3) size: number of elements currently in the array.
    e. Amortized Analysis: Aggregate Method:
        - start empty array and call n times pushback, $\bar{cost} = 1 + something -> O(1)$
    f. Amortized Analysis: Banker's Method:
        - Charge extra for each cheap operation.
        - Charge 3 for each insertion: 1coin is the raw cost for insertion, use token as prepaid resized cost.
    g. Amortized Analysis: Physicist's Method:
        - Define a potential function, $\theta$ which maps states of the data structure to integers:
        - adding element cost: 3
    h. *Important*: double is an import number, if not, we can't make the cost constant.

## Week 2
1. Priority Queues
    a. Def: 
        - A queue is an abstract data type supporting the following main operations: 1) `PushBack(e)` 2) `PopFront`
        - a priority queue is a queue with not position concept, it has only two main operations: 1) `insert(e)` 2) `getMaxPrivority`
        - it may have additional operations: 1) `remove(e)` remove an element e 2) `getmax()` get the element with the maximum privority without remove it from the privority queue 3) `changePrivority(e, p)` change the privority of element e to p.
        - slightly difference: sorted list VS. unsorted list
    b. applications:
        - Dijsktra's algorithm: finding shortest path
        - Prim's algorithm: constructing a minimum spanning tree of a graph
        - Huffman's algorithm: constructing an optimum prefix-free encoding of a string
        - Heap sort: sorting a given sequence(O($nlog(n)$), and sorting in place)
2. Priority Queues: Binary max-heap tree (the value of each node is at least the values of its children)
    a. insertion: 不断向上交换，直到达到刚好稳定的状态 O(n)
    b. extractMax: 与最外面的叶交换，然后不断调换root直至稳定O(n)
3. Complete Binary Trees:
    a. A binary tree is complete if all its levels are filled except possibly the last one which is filled from *left to right*. Reason: just try to keep the tree as small as possible
    b. Property 1: A complete binary tree with n nodes has height at most O(log n)
    c. Property 2: It can store as an arry because of it has ordering.
        - if we mark the root as level 1 and order 1, then we have: parent(i) = roundDown(i / 2), leftchild(i) = 2i, rightchild(i) = 2i+1。
        - so a complete tree with n nodes, must have levels roundDown(log(2, n))+1, and *the second last level must start from roundDown(n / 2)*
    d. Property 3: maxSize is the maximum number of elements in the heap. size if the size of heap. H[1..maxSize] is an array of length **maxSize** where the heap occupies the first size elements.
        - `Parent(i)`: return (roundDown(i / 2))
        - `LeftChild(i)`: return 2i
        - `RightChild(i)`: return 2*i+1
        
        ```pseudocode
        # ShiftUp(i)
        while i > 1 and H[Parent(i)] < H[i]:
            swap H[Parent(i)] and H[i]
            i <- Parent(i)
        
        # ShiftDown(i)
        maxIndex <- i
        l <- LeftChild(i)
        if l <= size and H(l) > H[maxIndex]
            maxIndex <- l
        r <- RightChild(i)
        if r <= size and H[r] > H[maxIndex]
            maxIndex <- r
        if i != maxIndex
            swap H[i] and H[maxIndex]
            ShiftDown(maxIndex)
        
        # Insert(p)
        if size = maxSize:
            return ERROR
        size <- size + 1
        H[size] <- p
        ShiftUp(size)
        
        # ExtractMax()
        result <- H[1]
        H[1] <- H[size]
        size <- size-1
        ShiftDown(1)
        return result
        
        # Remove(i)
        H[i] <- inf
        ShiftUp(i)
        ExtractMax()
        
        # ChangePriority(i, p)
        oldp <- H[i]
        H[i] <- p
        if p > oldp: 
            ShiftUp(i)
        else:
            ShiftDown(i)
        ```
    e. Summary:
        - *fast*: all operations work in time O(log n) (and `GetMax` even works in O(1))
        - *space efficient*: all operations happen in the obj itself
        - easy to implement
        - can be extended in many different ways(like d-ary complete trees)
4. Heap Sort

    ```HeapSort(A[1..n])
    create an empty priority queue
    for i from i to n:
        Insert(A[i])
    for i from n downto 1:
        A[i] <- ExtractMax()
    ```
    
    this algorithm is comparsion-based and has running time O(n log n)
    
    **as if we already have A in place, we can imporve it**:
    
    ```BuildHeap(A[1..n])
    size <- n
    for i from (roundDown (n/2)) downto 1:
        ShiftDown(i)
    ```
    
    we repair the heap property going from bottom to top, the running time is O(n log n)
    
    ```PartialSorting(A[1..n], k)
    BuildHeap(A)
    for i from 1 to k:
        ExtractMax()
    ```
    
    if k = O(n / (log n)), this algorithm can be solved in O(n)
5. Disjoint Sets
    a. Def: A disjoint-set data structure supports the following operations: `MakeSet(x)` creates a singleton set {x}; `Find(x)` returns ID of the set containing x: if x and y lie in the same set, then Find(x) = Find(y), otherwise Find(x) != Find(y); `Union(x, y)` merge the sets contain x and y
    
    ```Preprocess(maze)
    for each cell c in maze:
        MakeSet(c)
    for each cell c in maze:
        for each neighbor n of c:
            Union(c, n)
    ```
    
    b. Naive Implementation
    
    ```
    # MakeSet(i)
        smallest[i] <- i
        
    # Find(i)
        return smallest[i]
    
    # Union(i, j)
    i_id <- Find(i)
    j_id <- Find(j)
    if i_id = j_id
        return
    m <- min(i_id, j_id)
    for k from 1 to n:
        if smallest[k] in {i_id, j_id}
            smallest[k] <- m
    # running time: O(n)
    ```

    c. use link list can implemented merge effeciently. 
        - we can even imporve this by change the link behavior.
        - we can improve this by store it in a tree
    d. Represent each set as a tree: id is the root of the tree.
    
    ```
    # MakeSet(i)
        parent[i] <- i
        rank[i] <- 0
        
    # Find(i)
    while i != parent[i]:
        i <- parent[i]
    return i
    
    # Union(i, j)
    i_id <- Find(i)
    j_id <- Find(j)
    if i_id = j_id
        return
    if rank[i_id] > rank[j_id]:
        parent[j_id] <- i_id
    else:
        parent[i_id] <- j_id
        if rank[i_id] = rank[j_id]
            rank[j_id] <- rank[j_id] + 1
            % 由于这是递归调用，所以所有的点都会同步跟新相应的rank
    ```
    
        - *Important property*: for any node i, rank[i] is equal to the height of the tree rooted at i.
        - The height of any tree in the forest is at most $log(2,n)$
        - this lemma insures that *Union and Find work in time O($log n$)*
    e. **PATH Compression**
        - for any node i: rank[i] <= rank[parent[i]] 
        - once an internal node, forever internal node.

## Week 3
