# Examples
Here you can find some examples of lambda calculus to put into lcalc and try out.

## Identity
```
(\x.x)(\x.x) 0 -> 0
```

## Omega
```
(\x.x x)(\x.x x) -> (\x.x x)(\x.x x)
```

## True
```
(\x.(\y.x)) a b -> a
```

## False
```
(\x.(\y.y)) a b -> b
```

## Y-combinator
```
(\f.(\x.f (x x))(\x.f (x x))) F -> F ((\x.F (x x)) (\x.F (x x))) -> ...
```

## Renaming
```
(\x.(\y.x y))(\x.(\y.x y)) -> (\y.(\y1.y y1))
```

## Successor (S (S 0) = 2)
```
(\n.(\s.(\z.s (n s z)))) ((\n.(\s.(\z.s (n s z)))) (\s.(\z.z))) -> (\s.(\z.s (s z)))
```
