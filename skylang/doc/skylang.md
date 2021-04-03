**F#**关键字实在是太多了，给人的感觉是里面有很多不必要的东西。

因为F#搜到了*scala*这门语言，初看之下挺喜欢它的，直到我发现它用圆括号()来做下标运算，便一下子失去了好感，我第一次见使用圆括号`()`来做下标运算的编程语言是**Matlab**，我感觉这些语言太没功德了。

*scala*语言还有一个令人讨厌的地方，像`list.reverse`, `list.sorted`这样的函数，调用却不用括号，而`string.length`这种更加接近属性的却需要使用括号来调用！

我认为我们在设计一门新的编程语言时，若非必要，其语法应保持与之前最能够被广为接受的语言一致，要尽量尊重大众的审美，然后在此基础上进行功能的扩展。


正则表达式：
```scheme
(import re)
(define regexp r"(x|y)")
(define regex (re.compile regexp))
(define match (re.match regex "x and y"))
```

字符串格式化：
```scheme
(define variable 10)
(define fn (lambda (x y) (+ x y)))
(define x 1)
(define y 1)

f"{variable}"  ;; => 10
f"{(fn x y)}"  ;; => 2
```
