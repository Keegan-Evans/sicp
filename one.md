# Chapter 1

## 1.2

### ex 1.8

```

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

```