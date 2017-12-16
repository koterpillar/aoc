generate factor value = (value * factor) `mod` 2147483647

generateA = generate 16807

generateB = generate 48271

seqA = tail . iterate generateA

seqB = tail . iterate generateB

judge v = v `mod` 0x10000

judgeSame a b = judge a == judge b

divisibleBy a b = b `mod` a == 0
