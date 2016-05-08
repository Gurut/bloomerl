A Bloom Filter is a space-efficient probabilistic data structure that is used to test whether an element is a member of a set.

# Usage #
```
B0 = bloom:new(2000, 0.001).
bloom:is_bloom(B0).
B1 = bloom:add_element(Key, B0).
bloom:is_element(Key, B1).
```