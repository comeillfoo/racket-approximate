Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №3__

по Функциональному программированию

Выполнил: Ханнанов Л. И.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.

---

## Требования к разработанному ПО

* программа должна быть реализована в функциональном стиле
* ввод/вывод должен быть отделён от алгоритмов аппроксимации
* алгоритм аппроксимации должен порождать замыкания, работающие на некоторых интервалах
* требуется использовать идиоматичный для технологии стиль программирования
* реализация всех алгоритмов аппроксимации по варианту + аппроксимация отрезками (работает только на интервале)
* возможность настройки генератора аппроксимируемых точек и выбора нескольких алгоритмов аппроксимации
* входные и выходные данные подаются в формате CSV
* Unit-тесты и CI

## Ключевые элементы реализации с минимальными комментариями

### Код генератора аппроксимирующих точек

Генерирует с шагом step значения, начиная с start.

```racket
([x0
  (in-generator
    (let loop ([x (- (start) (* 2 (step)))])
      (begin
        (yield x)
        (loop (+ x (step))))))]
  ...)
```

### Алгоритмы аппроксимации

Общая функция, которая методом Крамера находит оптимальные коэффициенты для функций вида
$f(x)=ax + b$.
```racket
(define (linear-coefficients N SX SXX SY SXY)
  (let*
    ([delta (lambda (arg1 arg2 arg3 arg4) (- (* arg1 arg2) (* arg3 arg4)))]
     [delta0 (delta SXX N SX SX)]
     [delta1 (delta SXY N SX SY)]
     [delta2 (delta SXX SY SX SXY)]
     [a (/ delta1 delta0)]
     [b (/ delta2 delta0)])
    (cons a b)))
```

Остальные алгоритмы просто используют функцию выше, для нахождения коэффициентов.
```racket
(define (linear N SX SXX SY SXY)
  (let*
    ([coefficients (linear-coefficients N SX SXX SY SXY)]
    [a (car coefficients)]
    [b (cdr coefficients)])
    (lambda (x) (+ (* a x) b))))
```

Только алгоритм квадратичной аппроксимации, так как требует больше коэффициентов, потому что аппроксимирует функцией $f(x)=ax^2+bx+c$. Поэтому здесь решается система из двух уравнений, а не из трёх, как в остальных методах.
```racket
(define (quadratic N SX SXX SY SXY SXXX SXXXX SXXY)
  (with-handlers
    ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let*
      ([coefficients
        (array->list
          (matrix-solve
            (matrix [[N SX SXX] [SX SXX SXXX] [SXX SXXX SXXXX]])
            (col-matrix [SY SXY SXXY])))]
      [a0 (first coefficients)]
      [a1 (second coefficients)]
      [a2 (third coefficients)])
      (lambda (x) (+ a0 (* a1 x) (* a2 x x))))))
```

Аппроксимация происходит функцией $f(x)=ae^{bx}$, которую можно представить, как $log (f(x))=bx+ log(a)$, поэтому можно использовать функцию выше для нахождения коэффициентов линейной функции и преобразовать к экспоненциальной.
```racket
(define (exponential N SX SXX SLnY SXLnY)
  (let*
    ([coefficients (linear-coefficients N SX SXX SLnY SXLnY)]
      [A (car coefficients)]
      [B (cdr coefficients)]
      [a (exp B)]
      [b A])
    (lambda (x) (* a (exp (* b x))))))
```

Аппроксимация происходит функцией $f(x)=alog(x) + b$.
```racket
(define (logarithmic N SLnX SLnX2 SY SLnXY)
  (let*
    ([coefficients (linear-coefficients N SLnX SLnX2 SY SLnXY)]
     [a (car coefficients)]
     [b (cdr coefficients)])
    (lambda (x)
      (if (> x 0)
        (+ (* a (log x)) b)
        +nan.0))))
```

Аппроксимация происходит функцией $f(x)=ax^b$.
```racket
(define (power N SLnX SLnX2 SLnY SLnXLnY)
  (let*
    ([coefficients (linear-coefficients N SLnX SLnX2 SLnY SLnXLnY)]
     [A (car coefficients)]
     [B (cdr coefficients)]
     [a (exp B)]
     [b A])
    (lambda (x)
      (if (zero? x)
        +nan.0
        (* a (expt x b))))))
```

Используется формула прямой:
$$
  \frac{x - x_0}{x_1 - x_0} = \frac{y - y_0}{y_1 - y_0}\Rightarrow
$$
$$
  y = \frac{x - x_0}{x_1 - x_0}(y_1 - y_0) + y_0
$$
```racket
;;; Ищем отрезок
;;; x0    x    x1
;;; |-----|----|
(define (segment xs ys)
  (if (or (null? xs) (null? ys))
    (lambda (x) +nan.0)
    (let*
      ([left-bound  (apply min xs)]  ;;; минимальный x
       [right-bound (apply max xs)]) ;;; максимальный x
      (lambda (x)
        (if (or (< x left-bound) (> x right-bound))
          +nan.0 ;;; за пределами значение не определено
          (let*
            ;;; x0 - минимальное значение, разница между x и x0 минимальна
            ([x0
              (argmin
                (lambda (maybe-x) (- x maybe-x))
                (filter-not (lambda (arg) (> arg x)) xs))]
            [y0 (list-ref ys (index-of xs x0))]  ;;; f(x0) -> y0
            [x1 (last (member x0 xs))] ;;; правая граница отрезка
            [y1 (list-ref ys (index-of xs x1))]) ;;; f(x1) -> y1
            (if (eq? x0 x1)
              y0
              (+ y0 (* (/ (- y1 y0) (- x1 x0)) (- x x0))))))))))
```

### Ввод программы

```racket
(define table
  (for/stream
    ([line (in-lines)])
    (let ([xy (string-split line ";" #:trim? #t #:repeat? 2)])
      (cons
        (string->number (first xy))
        (string->number (second xy))))))
```

### Вывод программы

Чтобы оптимизировать вычисления сумм подаваемых на разные алгоритмы, было решено накапливать их в одном цикле for/fold, который обходит генерируемые `x0`, пока их число `n` не превысит значение параметра count (по умолчанию `+inf.0`).

Обход подаваемых данных происходит с помощью функций `more?` и `get`. С помощью которых решается, обновлять ли текущие суммы новыми значениями или нет.
```racket
(module+ main
  (print-header)
  (let*-values
    ([(more? get) (sequence-generate table)])

    (for/fold

      ;;; accums
      ([N       0]
       [SX      0]
       [SXX     0]
       [SXXX    0]
       [SXXXX   0]
       [SLnX    0]
       [SLnX2   0]
       [SY      0]
       [SLnY    0]
       [SXLnY   0]
       [SLnXY   0]
       [SXY     0]
       [SLnXLnY 0]
       [SXXY    0]
       [Xs   null]
       [Ys   null]
       [n       0]
       #:result (void))

      ;;; looped-structure
      ([x0
        (in-generator
          (let loop ([x (- (start) (* 2 (step)))])
            (begin
              (yield x)
              (loop (+ x (step))))))]
        #:break (>= n (count)))

      (when
        (> N 1)
        ;;; body
        (pretty-print x0)
        (when
          (linear-enabled)
          (pretty-print #:sep ";"
            ((linear N SX SXX SY SXY) x0)))
        (when
          (quad-enabled)
          (pretty-print #:sep ";"
            ((quadratic N SX SXX SY SXY SXXX SXXXX SXXY) x0)))
        (when
          (exp-enabled)
          (pretty-print #:sep ";"
            ((exponential N SX SXX SLnY SXLnY) x0)))
        (when
          (log-enabled)
          (pretty-print #:sep ";"
            ((logarithmic N SLnX SLnX2 SY SLnXY) x0)))
        (when
          (pow-enabled)
          (pretty-print #:sep ";"
            ((power N SLnX SLnX2 SLnY SLnXLnY) x0)))
        (when
          (seg-enabled)
          (pretty-print #:sep ";"
            ((segment Xs Ys) x0)))
        (newline))
      (if (more?)
        (let*
          ([xy (get)]
           [x (car xy)]
           [y (cdr xy)])

          (values
            (add1 N)
            (+ SX x)
            (+ SXX (* x x))
            (+ SXXX (expt x 3))
            (+ SXXXX (expt x 4))
            (+ SLnX (log x))
            (+ SLnX2 (expt (log x) 2))
            (+ SY y)
            (+ SLnY (log y))
            (+ SXLnY (* x (log y)))
            (+ SLnXY (* (log x) y))
            (+ SXY (* x y))
            (+ SLnXLnY (* (log x) (log y)))
            (+ SXXY (* x x y))
            (append Xs (list x))
            (append Ys (list y))
            (add1 n)))
        (values N SX SXX SXXX SXXXX SLnX SLnX2 SY SLnY SXLnY SLnXY SXY SLnXLnY SXXY Xs Ys (add1 n))))))
```

## Ввод/вывод программы

power_sqrtx.csv:
```csv
5.366666666666666;2.3166067138525404
10.733333333333333;3.2761766334148303
16.099999999999998;4.0124805295477755
21.466666666666665;4.633213427705081
26.833333333333332;5.180090089306685
32.199999999999996;5.674504383644442
37.56666666666666;6.129165250396392
42.93333333333333;6.552353266829661
48.3;6.9498201415576215
53.666666666666664;7.32575365861197
59.03333333333333;7.683315256667094
64.39999999999999;8.024961059095551
69.76666666666665;8.352644291879468
75.13333333333331;8.667948623136464
80.49999999999997;8.97217922246318
85.86666666666663;9.26642685541016
91.23333333333329;9.55161417422905
96.59999999999995;9.82852990024449
101.96666666666661;10.097854557611068
107.33333333333327;10.360180178613366
112.69999999999993;10.616025621672167
118.06666666666659;10.86584863996672
123.43333333333325;11.1100555054119
128.79999999999993;11.349008767288883
134.1666666666666;11.5830335692627
139.53333333333327;11.81242283925416
144.89999999999995;12.037441588643325
150.26666666666662;12.258330500792782
155.6333333333333;12.475308947410213
160.99999999999997;12.688577540449518
```

Соответствующий вывод.
```bash
comeillfoo@come1llf00:~/fun/functional-programming/racket-approximate$ cat tests/power_sqrtx.csv | racket approximator.rkt -lqegps --step 0.7 --start 20.0 -c 8
              x;         linear;      quadratic;       exponent;      logarithm;          power;        segment
     2.0000E+01;     4.9331E+00;     3.6292E+00;     5.9602E+00;     4.1378E+00;     4.4721E+00;               
     2.0700E+01;     4.7765E+00;     4.4659E+00;     5.2007E+00;     4.3507E+00;     4.5497E+00;               
     2.1400E+01;     4.7030E+00;     4.6214E+00;     4.8397E+00;     4.5312E+00;     4.6260E+00;     4.6255E+00
     2.2100E+01;     4.6757E+00;     4.7261E+00;     4.6531E+00;     4.6903E+00;     4.7011E+00;     4.6978E+00
     2.2800E+01;     4.6766E+00;     4.8064E+00;     4.5568E+00;     4.8343E+00;     4.7749E+00;     4.7626E+00
     2.3500E+01;     4.6958E+00;     4.8740E+00;     4.5125E+00;     4.9672E+00;     4.8477E+00;     4.8221E+00
```


## Выводы

