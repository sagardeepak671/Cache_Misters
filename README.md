# COL216 ASSIGNMENT 3


## Cache Read

### Read hit : 
 - No change in any state
 - E/M/S : Simple , takes place in single cycle

### Read Miss:
- First we find the Least Recently used cache line
- If the LRU line is in `M` state then we write it back to the memory before evicting it(100 cycles used for transfer)
- If the LRU is not in `M` state then we simply evict it.
- Waits for bus to be free before sending a snoop request.
- If bus is busy we stall the core.

- required address found in other cache:
  
  |State of the cache line in other cache| | Number of cycles|
  |--------------------------------------|-|-|
  | M |copy to requesting cache, write it back to memory , update state of both cache lines to S | 2\*n + 100 (2\*n for cache to cache copy,100 for memory access) |
  | E | copy to requesting cache , update state of both cache lines to S | 2 * n |
  | S | copy to requesting cache , update state of requesting cache to S | 2 * n |

- required address not found in other caches:
  - We bring the data from the memory (takes 100 cycles) and update the  state of the requesting cache line to E .
 
## Cache Write
### Write Hit:
   |State of the cache line| | Number of cycles|
    |-|-|-|
    | M | no change in state | 1 |
    | E | change state to `M` | 1 |
    | S | wait for bus to be free , then invalidate the other S cache lines with the same tag in the other caches , update the current cache line to `M` | 1 (if bus is free when the instruction is called) |

### Write Miss:
-  Again just like in the case of Read miss , we start by finding the LRU
- If the LRU line is in `M` state then we write it back to the memory before evicting it(100 cycles used for transfer)
- If the LRU is not in `M` state then we simply evict it.
- Waits for bus to be free before sending a snoop request.
- If bus is busy we stall the core.

- required address found in other cache:
   |State of the cache line in other cache| | Number of cycles|
    |--------------------------------------|-|-|
   | M | write back to memory , then invalidate the cache line in the other cache , then read from memory to the requesting cache updating the cache line in the requesting cache to `M` | 200 (100 for write back and 100 for reading it into the requesting cache|
  | E/S | Value read into the local cache from memory , Invalidate all other cache lines with E/S states , then set the local cache line to `E` state | 100 |

- required address not found in other cache :
    - We read the data from the memory and then write the new data to the cache and update the local cache line state to `M`
