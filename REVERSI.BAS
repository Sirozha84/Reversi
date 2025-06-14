DECLARE FUNCTION Menu! ()
DECLARE SUB NewGame (lvl!, sz!)
DECLARE FUNCTION MiniMenu! (menutype!)
DECLARE SUB RedWindow (x1!, y1!, x2!, y2!)
DECLARE SUB DrawTable (pl!)
DECLARE SUB Benchmark ()
DECLARE SUB Delay (s!)
DECLARE FUNCTION WhoWin! (pass!)
DECLARE SUB CandidatesCalc (pl!)
DECLARE SUB Win (w!)
DECLARE FUNCTION FCount! (pl!)
DECLARE SUB Game (gr!(), move!)
DECLARE FUNCTION StepComputer! (pl!)
DECLARE SUB CondidatesCalc (pl!)
DECLARE SUB StepEnd (cx!, cy!, pl!)
DECLARE SUB CellFill (cx!, cy!, mx!, my!, pl!)
DECLARE FUNCTION CellCheck! (x!, y!, mx!, my!, pl!)
DECLARE SUB CondidateCalc (pl!)
DECLARE FUNCTION StepHuman! (gr!(), col!)
DECLARE SUB Init ()
DECLARE SUB Load ()
DECLARE SUB LoadGRP (m!(), file AS STRING, cn!, ln!)

DIM SHARED kup$: kup$ = CHR$(0) + CHR$(72)
DIM SHARED kdn$: kdn$ = CHR$(0) + CHR$(80)
DIM SHARED klf$: klf$ = CHR$(0) + CHR$(75)
DIM SHARED krt$: krt$ = CHR$(0) + CHR$(77)
DIM SHARED ken$: ken$ = CHR$(13)
DIM SHARED kes$: kes$ = CHR$(27)

REDIM SHARED grf(0, 0)  'Фишки                40*32*2
REDIM SHARED grf8(0, 0) 'Фишки для поля 8x8   40*32*9
REDIM SHARED grf16(0, 0)'Фишки для поля 16x16 20*16*9
REDIM SHARED grf32(0, 0)'Фишки для поля 32x32 10*12*9
REDIM SHARED grc(0, 0)  'Цветовые градации    20*16*56
REDIM SHARED grk(0, 0)  'Кнопки меню          200*25*14
REDIM SHARED gra(0, 0)  'Стрелки меню         46*24*11
REDIM SHARED grn(0, 0)  'Буквы названия       46*46*7
REDIM SHARED grp(0, 0)  'Полоса прокрутки     20*16*6
REDIM SHARED grt(0, 0)  'Табло                94*322*1

DIM SHARED m(31, 31)    'Игровое поле
DIM SHARED level        'Уровень сложности
DIM SHARED size         'Размер
DIM SHARED pl(2) AS STRING   'Имена игроков
DIM SHARED left
DIM SHARED bott
DIM SHARED wd
DIM SHARED hg
DIM SHARED kx           'Курсор
DIM SHARED ky
DIM SHARED c(31, 31)    'Матрица для вычисления кодидатов
DIM SHARED cicles       'Размер паузы
DIM SHARED kmm          'Курсор главного меню

Init
DO
LOOP UNTIL Menu

SUB Benchmark
    cicles = 0
    t = TIMER
    DO
    LOOP UNTIL t <> TIMER
    t = TIMER
    DO
        cicles = cicles + 1
    LOOP UNTIL t <> TIMER
END SUB

'Вычисление матрицы кандидатов
'0 - Если хода нет, 1 и более - количество "обращённых" противников
SUB CandidatesCalc (pl)
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) <> 0 THEN
                c(x, y) = 0
            ELSE
                c = CellCheck(x, y, 0, 1, pl)
                c = c + CellCheck(x, y, 1, 1, pl)
                c = c + CellCheck(x, y, 1, 0, pl)
                c = c + CellCheck(x, y, 1, -1, pl)
                c = c + CellCheck(x, y, 0, -1, pl)
                c = c + CellCheck(x, y, -1, -1, pl)
                c = c + CellCheck(x, y, -1, 0, pl)
                c = c + CellCheck(x, y, -1, 1, pl)
                c(x, y) = c
            END IF
        NEXT
    NEXT
END SUB

'Расчёт количества "обращённых" в клетке cx, cy
'mx, my - направление
'pl - игок
FUNCTION CellCheck (cx, cy, mx, my, pl)
    x = cx
    y = cy
    pr = 3 - pl
    c = 0
    ex = 0
    DO
        x = x + mx
        y = y + my
        IF x < 0 OR y < 0 OR x > size - 1 OR y > size - 1 THEN
            ex = -1
        ELSE
            IF m(x, y) = 0 THEN EXIT FUNCTION
            IF m(x, y) = pr THEN c = c + 1
            IF m(x, y) = pl THEN CellCheck = c: EXIT FUNCTION
        END IF
    LOOP WHILE NOT ex
END FUNCTION

'Закраска чужих фишек по линии
SUB CellFill (cx, cy, mx, my, pl)
    c = CellCheck(cx, cy, mx, my, pl)
    FOR i = 0 TO c
        x = cx + mx * i
        y = cy + my * i
        m(x, y) = pl
    NEXT
END SUB

'Пауза в секундах
SUB Delay (s)
    FOR i = 0 TO s * cicles * 18
        t = TIMER
    NEXT
END SUB

'Перерисовка данных табло
SUB DrawTable (pl)
    k = 94 / size / size
    COLOR 11
    FOR i = 0 TO 1
        p = 2 - i
        s = FCount(1 + i)
        a = FCount(2 - i)
        LOCATE 13, 4 + i * 68
        PRINT s
        LOCATE 17, 4 + i * 68
        PRINT s - a
        x = i * 546
        LINE (x, 42)-(x + 93, 55), 15, B
        LINE (x, 42)-(x + s * k, 55), 11 - (1 + i), BF
        LINE (x + s * k, 42)-(x + (s + a) * k, 55), 11 - (2 - i), BF
    NEXT
    COLOR 11 - pl
    LOCATE 1, 1
    PRINT TAB(INT(41 - (LEN(pl(pl)) + 7) / 2));
    PRINT "Ходит: " + pl(pl); TAB(80);
END SUB

'Подсчёт количества фишек
FUNCTION FCount (pl)
    cn = 0
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) = pl THEN cn = cn + 1
        NEXT
    NEXT
    FCount = cn
END FUNCTION

'Процедура игры
SUB Game (gr(), move)

    'Определяем размеры полей и фишек
    SELECT CASE size
        CASE 8:
            left = 110
            bott = 15
            wd = 54
            hg = 41
        CASE 16:
            left = 100
            bott = 17
            wd = 28
            hg = 20
        CASE 32:
            left = 98
            bott = 16
            wd = 14
            hg = 10
    END SELECT

    'Рисуем панели
    CLS
    LOCATE 25, 29
    COLOR 7
    PRINT "Пробел - пас, Esc - меню"
    FOR i = -226 TO 544 STEP 2
        LINE (i, 14)-(i + 321, 335), 8
    NEXT
    FOR i = 0 TO 1
        PUT (i * 546, 14), grt(0, 0), PSET
        PUT (27 + i * 546, 277), grf(0, i), PSET
    NEXT
    
    COLOR 14
    FOR i = 0 TO 1
        LOCATE 9, 7 + i * 68 - LEN(pl(i + 1)) / 2
        PRINT pl(i + 1)
    NEXT

    'Рисуем поле
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            PUT (left + x * wd, bott + y * hg), gr(0, m(x, y)), PSET
        NEXT
    NEXT

    'Игровой цикл
    DIM nm(size - 1, size - 1)
    pass = 0
    endgame = 0
    DO
        'Обновляем табло
        DrawTable move

        'Определяем, кто сейчас ходит
        IF pl(move) <> "Computer" THEN
            s = StepHuman(gr(), move)
        ELSE
            s = StepComputer(move)
        END IF
        IF s = 1 THEN pass = pass + 1 ELSE pass = 0 'Два пасса
        IF s = 2 THEN endgame = -1                  'Запрос выхода

        'Обновляем поле
        FOR x = 0 TO size - 1
            FOR y = 0 TO size - 1
                IF first OR nm(x, y) <> m(x, y) THEN
                    nm(x, y) = m(x, y)
                    PUT (left + x * wd, bott + y * hg), gr(0, m(x, y)), PSET
                END IF
            NEXT
        NEXT
        
        'Определяем, есть ли победа
        w = WhoWin(pass > 1)
        IF w > 0 THEN
            Win (w)
            endgame = -1
        END IF

        'Переход хода
        move = 3 - move

    LOOP WHILE NOT endgame
    IF w > 0 THEN LOCATE 15, 30: PRINT "Игра окончана"
END SUB

'Инициализация, загрузка файлов графики
SUB Init
    SCREEN 9
    Benchmark
    WINDOW (0, 0)-(639, 349)
    LoadGRP grf(), "f.grp", 2, 281
    LoadGRP grf8(), "f8.grp", 9, 281
    LoadGRP grf16(), "f16.grp", 9, 81
    LoadGRP grf32(), "f32.grp", 9, 21
    LoadGRP grc(), "color.grp", 99, 49
    LoadGRP grk(), "keys.grp", 14, 626
    LoadGRP gra(), "kursor.grp", 11, 145
    LoadGRP grn(), "name.grp", 7, 277
    LoadGRP grp(), "prok.grp", 6, 49
    LoadGRP grt(), "tab.grp", 1, 3865
    kmm = 6
END SUB

'Загрузка спрайтов
'm() - массив, содержащий спрайт
'file - имя файла
'cn - колиочество спрайтов
'ln - длина массива
SUB LoadGRP (m(), file AS STRING, cn, ln)
    REDIM m(ln - 1, cn - 1)
    OPEN file FOR RANDOM AS 1 LEN = 4
    i = 1
    FOR n = 0 TO cn - 1
        FOR d = 0 TO ln - 1
            GET #1, i, m(d, n)
            i = i + 1
        NEXT
    NEXT
    CLOSE
END SUB

FUNCTION Menu
    CLS
    LOCATE 25, 76
    COLOR 8
    PRINT "V 1.1"
    COLOR 15
    FOR i = 0 TO 6
        PUT (27 + i * 90, 280), grn(0, i), PSET
    NEXT

    FOR i = 0 TO 9
        y = i * .6
        LINE (100 + i, 10 + y)-(100 + i, 239 - y)
        LINE -(539 - i, 239 - y)
        LINE -(539 - i, 10 + y), 8
        LINE -(100 + i, 10 + y), 8
    NEXT
    LINE (110, 16)-(529, 233), 7, BF
    FOR i = 0 TO 6
        PUT (220, 22 + i * 30), grk(0, i), PSET
    NEXT
    DO
        PUT (140, 22 + kmm * 30), gra(0, INT(a)), PSET
        PUT (453, 22 + kmm * 30), gra(0, INT(a) + 5), PSET
        k$ = INKEY$
        IF k$ <> "" THEN
            LINE (140, 22 + kmm * 30)-(185, 45 + kmm * 30), 7, BF
            LINE (453, 22 + kmm * 30)-(498, 45 + kmm * 30), 7, BF
        END IF
        IF k$ = kup$ THEN kmm = kmm + 1
        IF k$ = kdn$ THEN kmm = kmm - 1
        IF kmm > 6 THEN kmm = 0
        IF kmm < 0 THEN kmm = 6
        a = a + .2
        IF a >= 5 THEN a = 0
        Delay .01
    LOOP UNTIL k$ = ken$
    PUT (220, 22 + kmm * 30), grk(0, kmm + 7), PSET
    Delay .15
    PUT (220, 22 + kmm * 30), grk(0, kmm), PSET
    Delay .15

    'Новая игра
    IF kmm = 6 THEN
        k = MiniMenu(1)
        IF k <> -1 THEN
            s = 8
            IF k = 1 THEN s = 16
            IF k = 2 THEN s = 32
            k = MiniMenu(2)
        END IF
        IF k <> -1 THEN
            l = 0
            IF k = 1 THEN l = 1
            NewGame l, s
        END IF
    END IF

    'Новая игра для двух игроков
    IF kmm = 5 THEN
        k = MiniMenu(1)
        IF k <> -1 THEN
            s = 8
            IF k = 1 THEN s = 16
            IF k = 2 THEN s = 32


            NewGame l, s
        END IF
    END IF

    Menu = (kmm = 0)
END FUNCTION

FUNCTION MiniMenu (menutype)
    DIM p(20) AS STRING
    SELECT CASE menutype
        CASE 0
            p(0) = " Продолжить (Esc)"
            p(1) = "Сохранить и выйти"
            p(2) = "      Выйти"
            max = 2
            ex = 0
        CASE 1
            tit$ = "   Выберите размер поля"
            p(0) = "       8x8"
            p(1) = "      16x16"
            p(2) = "      32x32"
            max = 2
            ex = -1
        CASE 2
            tit$ = "Выберите уровень сложности"
            p(0) = "    Нормальная"
            p(1) = "     Сложная"
            max = 1
            ex = -1
    END SELECT
    rows = max + 1
    sh = 0
    IF tit$ <> "" THEN sh = 1   'Сдвиг, если есть тайтл
    rows = rows + sh
    top = 13 - INT(rows / 2)
    x = 150
    y = 350 - (top + rows - 1) * 14 - 7
    h = (rows) * 14 - 1 + 14
    PCOPY 0, 1
    FOR i = 1 TO -.01 STEP -.1
        RedWindow x + i * 170, y + i * h / 2, 639 - x - i * 170, y + h - i * h / 2
        Delay .01
    NEXT
    DO
    LOOP UNTIL INKEY$ = ""
    k = 0
    DO
        LOCATE top, 28
        COLOR 9
        PRINT tit$
        FOR i = 0 TO max
            LOCATE top + i + sh, 32
            COLOR 9
            IF k = i THEN COLOR 13
            PRINT p(i)
        NEXT
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
        IF k$ = kup$ THEN k = k - 1
        IF k$ = kdn$ THEN k = k + 1
        IF k < 0 THEN k = max
        IF k > max THEN k = 0
        IF k$ = kes$ THEN k = ex: k$ = ken$
    LOOP UNTIL k$ = ken$
    PCOPY 1, 0
    MiniMenu = k
END FUNCTION

'Запуск новой игры
SUB NewGame (lvl, sz)

    level = lvl
    size = sz
    
    'Подготовка поля
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            m(x, y) = 0
        NEXT
    NEXT
    m(size / 2, size / 2) = 1
    m(size / 2 - 1, size / 2 - 1) = 1
    m(size / 2, size / 2 - 1) = 2
    m(size / 2 - 1, size / 2) = 2

    'Поодготовка игроков
    pl(1) = "Сергей"
    pl(2) = "Computer"
    move = 1
    kx = size / 2 - 2
    ky = size / 2

    'Запуск нужного игры
    SELECT CASE size
        CASE 8:
            Game grf8(), move
        CASE 16:
            Game grf16(), move
        CASE 32:
            Game grf32(), move
    END SELECT

END SUB

SUB RedWindow (x1, y1, x2, y2)
    LINE (x1, y1)-(x2, y2), 13, B
    LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), 12, B
    LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), 4, B
    LINE (x1 + 3, y1 + 3)-(x2 - 3, y2 - 3), 0, BF
END SUB

'Ход компьютера
'0 - ход сделан
'1 - пас
FUNCTION StepComputer (pl)

    CandidatesCalc pl

    'Надо узнать не является ли какой-то ход решающим
    ac = FCount(3 - pl)
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF c(x, y) = ac THEN c(x, y) = 99
        NEXT
    NEXT


    'Приоритетные и не желательные позиции
    IF level > 0 THEN
        B = size - 1
        IF c(0, 0) > 0 THEN c(0, 0) = 98
        IF c(0, B) > 0 THEN c(0, B) = 98
        IF c(B, 0) > 0 THEN c(B, 0) = 98
        IF c(B, B) > 0 THEN c(B, B) = 98
        FOR i = 2 TO B - 2
            IF c(i, 0) > 0 THEN c(i, 0) = 97
            IF c(i, B) > 0 THEN c(i, B) = 97
            IF c(0, i) > 0 THEN c(0, i) = 97
            IF c(B, i) > 0 THEN c(B, i) = 97

        NEXT
    END IF

    'Определяем наилучшие позиции
    max = 0
    szs = size * size
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            cn = c(x, y)
            IF cn > max THEN
                max = cn
                REDIM cx(szs)
                REDIM cy(szs)
                n = 0
            END IF
            IF cn = max AND cn > 0 THEN
                cx(n) = x
                cy(n) = y
                n = n + 1
            END IF
        NEXT
    NEXT

    'Делаем ход, или пасуем
    IF max > 0 THEN
        s = INT(RND * n)
        StepEnd cx(s), cy(s), pl
    ELSE
        StepComputer = 1
    END IF

END FUNCTION

'Завершение хода - обращение фишек противника
SUB StepEnd (cx, cy, pl)
    CellFill cx, cy, 0, 1, pl
    CellFill cx, cy, 1, 1, pl
    CellFill cx, cy, 1, 0, pl
    CellFill cx, cy, 1, -1, pl
    CellFill cx, cy, 0, -1, pl
    CellFill cx, cy, -1, -1, pl
    CellFill cx, cy, -1, 0, pl
    CellFill cx, cy, -1, 1, pl
END SUB

'Ход человека
'0 - ход сделан
'1 - пас
'2 - выход
FUNCTION StepHuman (gr(), pl)
    CandidatesCalc pl
    DO
        x = left + kx * wd
        y = bott + ky * hg

        IF c(kx, ky) > 0 THEN
            PUT (x, y), gr(0, 5), AND
            PUT (x, y), gr(0, 2 + pl), XOR
        ELSE
            PUT (x, y), gr(0, 8), AND
            PUT (x, y), gr(0, 5 + pl), XOR
        END IF
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
    
        PUT (left + kx * wd, bott + ky * hg), gr(0, m(kx, ky)), PSET
        IF k$ = klf$ AND kx > 0 THEN kx = kx - 1
        IF k$ = krt$ AND kx < size - 1 THEN kx = kx + 1
        IF k$ = kdn$ AND ky > 0 THEN ky = ky - 1
        IF k$ = kup$ AND ky < size - 1 THEN ky = ky + 1
        IF k$ = ken$ AND c(kx, ky) > 0 THEN
            StepEnd kx, ky, pl
            EXIT FUNCTION
        END IF
        IF k$ = " " THEN
            StepHuman = 1
            EXIT FUNCTION
        END IF
        IF k$ = kes$ THEN
            k = MiniMenu(0)
            IF k = 1 THEN

                'Тут будет сохранение игры

            END IF
            IF k > 0 THEN
                StepHuman = 2
                EXIT FUNCTION
            END IF
        END IF
    LOOP WHILE -1
END FUNCTION

'Определение победителя
'0 - продолжаем играть        
'1,2 - победа
'3 - ничья
FUNCTION WhoWin (pass)
    p = 0
    p1 = 0
    p2 = 0
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) = 0 THEN p = 1
            IF m(x, y) = 1 THEN p1 = p1 + 1
            IF m(x, y) = 2 THEN p2 = p2 + 1
        NEXT
    NEXT
    IF p = 0 OR p1 = 0 OR p2 = 0 OR pass THEN
        WhoWin = 3
        IF p1 > p2 THEN WhoWin = 1
        IF p2 > p1 THEN WhoWin = 2
    ELSE
        WhoWin = 0
    END IF
END FUNCTION

SUB Win (w)
    LOCATE 13, 30
    IF w = 3 THEN
        PRINT "Ничья!"
    ELSE
        PRINT "Победил: "; pl(w)
    END IF
END SUB

